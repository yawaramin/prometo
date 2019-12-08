type error = [ | `Prometo_cancelled];

type t('a, 'e) = Js.Promise.t(result('a, [> error] as 'e));

exception Prometo_cancelled;

[@bs.get]
external cancelled: t('a, 'e) => option(bool) = "_prometo_cancelled";

let make = a => Js.Promise.resolve(Ok(a));

module Error = {
  let ok = make;
  let make = e => Js.Promise.resolve(Error(e));

  let flatMap = (~f, t) =>
    Js.Promise.then_(
      result =>
        switch (result, cancelled(t)) {
        | (_, Some(true)) => make(`Prometo_cancelled)
        | (Ok(_), _)
        | (Error(`Prometo_cancelled), _) => Obj.magic(t)
        | (Error(e), _) => f(e)
        },
      t,
    );

  let map = (~f, t) => flatMap(~f=e => e |> f |> make, t);
  let forEach = (~f, t) => t |> map(~f) |> ignore;
  let recover = (~f, t) => flatMap(~f=e => e |> f |> ok, t);
};

[@bs.set]
external cancel: (t('a, 'e), [@bs.as {json|true|json}] _) => unit =
  "_prometo_cancelled";

let updateResult = (result, elem) =>
  switch (result, elem) {
  | (_, Error(_) as error) => error
  | (Error(_), _) => result
  | (Ok(elems), Ok(elem)) =>
    elems |> Js.Array.push(elem) |> ignore;
    Ok(elems);
  };

let fromArray = array =>
  array
  |> Js.Promise.all
  |> Js.Promise.then_(results =>
       results
       |> Js.Array.reduce(updateResult, Ok([||]))
       |> Js.Promise.resolve
     );

let fromPromise = promise =>
  promise
  |> Js.Promise.then_(make)
  |> Js.Promise.catch(e => Error.make(`Prometo_error(e)));

let flatMap = (~f, t) =>
  Js.Promise.(
    then_(
      result =>
        switch (result, cancelled(t)) {
        | (_, Some(true)) => resolve(Error(`Prometo_cancelled))
        | (Ok(a), _) => f(a)
        | (Error(_), _) => Obj.magic(t)
        },
      t,
    )
  );

let map = (~f, t) => flatMap(~f=a => a |> f |> make, t);

let forEach = (~f, t) => t |> map(~f) |> ignore;

let toPromise = t =>
  Js.Promise.(
    then_(
      result =>
        switch (result, cancelled(t)) {
        | (_, Some(true))
        | (Error(`Prometo_cancelled), _) => reject(Prometo_cancelled)
        | (Ok(a), _) => resolve(a)
        | (Error(`Prometo_error(error)), _) => error |> Obj.magic |> reject
        | (Error(e), _) => e |> Obj.magic |> reject
        },
      t,
    )
  );
