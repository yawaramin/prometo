type error = [ | `Prometo_error(Js.Promise.error) | `Prometo_cancelled];
type t('a, 'e) = Js.Promise.t(result('a, [> error] as 'e));

exception Prometo_cancelled;

type cancelledSymbol;
[@bs.val] external cancelledSymbol: unit => cancelledSymbol = "Symbol";
let cancelledSymbol = cancelledSymbol();

[@bs.get_index]
external cancelled: (t(_, _), cancelledSymbol) => option(bool) = "";
let cancelled = t => cancelled(t, cancelledSymbol);

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
        | (Error(e), _) =>
          e |> f |> Js.Promise.catch(e => make(`Prometo_error(e)))
        },
      t,
    );

  let map = (~f, t) => flatMap(~f=e => e |> f |> make, t);
  let recover = (~f, t) => flatMap(~f=e => e |> f |> ok, t);
  let forEach = (~f, t) => t |> recover(~f) |> ignore;
};

[@bs.set_index]
external cancel: (t(_, _), cancelledSymbol, bool) => unit = "";
let cancel = t => cancel(t, cancelledSymbol, true);

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
  Js.Promise.then_(
    result =>
      switch (result, cancelled(t)) {
      | (_, Some(true)) => Error.make(`Prometo_cancelled)
      | (Ok(a), _) =>
        a |> f |> Js.Promise.catch(e => Error.make(`Prometo_error(e)))
      | (Error(err), _) => Error.make(err)
      },
    t,
  );

let map = (~f) => flatMap(~f=a => a |> f |> make);

let forEach = (~f, t) => t |> map(~f) |> ignore;

let thenPromise = (~f) => flatMap(~f=a => a |> f |> fromPromise);

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

module Infix = {
  let (>>=) = (t, f) => flatMap(~f, t);
  let (>|=) = (t, f) => map(~f, t);
};

open Infix;

let zip2 = (t1, t2) => t1 >>= (a1 => t2 >|= (a2 => (a1, a2)));

let zip3 = (t1, t2, t3) =>
  t1 >>= (a1 => t2 >>= (a2 => t3 >|= (a3 => (a1, a2, a3))));

let zip4 = (t1, t2, t3, t4) =>
  t1 >>= (a1 => t2 >>= (a2 => t3 >>= (a3 => t4 >|= (a4 => (a1, a2, a3, a4)))));

let zip5 = (t1, t2, t3, t4, t5) =>
  t1
  >>= (
    a1 =>
      t2
      >>= (
        a2 =>
          t3 >>= (a3 => t4 >>= (a4 => t5 >|= (a5 => (a1, a2, a3, a4, a5))))
      )
  );

let zip6 = (t1, t2, t3, t4, t5, t6) =>
  t1
  >>= (
    a1 =>
      t2
      >>= (
        a2 =>
          t3
          >>= (
            a3 =>
              t4
              >>= (
                a4 => t5 >>= (a5 => t6 >|= (a6 => (a1, a2, a3, a4, a5, a6)))
              )
          )
      )
  );
