## Prometo

A type-safe promise type for ReasonML, built directly on top of
JavaScript but adding fine-grained error control and promise
cancellation.

## How it works

Prometo is type-safe because of the following:

- It's not just a 'promise of your data', it's a 'promise of result of
  your data'. In other words, it can't be affected by JavaScript
  promises' well-known unsoundness issue where a 'promise of promise of
  data' is collapsed at runtime to a 'promise of data'.
- Also because a Prometo promise is a 'promise of result of data', it
  encodes an error at the type-level using the Reason `result('a, 'e)`
  type. In fact, Prometo ensures that its wrapped promises are not
  rejected, as long as you use its operations. So you can be sure that a
  Prometo promise is actually not going to throw at runtime. The only
  point at which you need to care about catching a possible exception is
  when converting it back into a normal JavaScript promise.

Because it's just a JavaScript promise wrapper, it's also easy to
convert back and forth between JavaScript and Prometo promises.

## Fine-grained error management

Using the technique described in
[Composable Error Handling in OCaml](https://keleshev.com/composable-error-handling-in-ocaml),
Prometo exposes an error type `'e` directly in its main polymorphic
promise type `Prometo.t('a, 'e)`. This allows you to explicitly track
and manage errors at every point of the code where you use these
promises.

## Interoperability

It's easy to interop with JavaScript promises:

- Use `fromPromise` to convert from a JavaScript Promise to a Prometo
  promise
- `toPromise` to convert from Prometo to a JavaScript promise
- `thenPromise` to chain together a Prometo promise and a function that
  returns a JavaScript Promise, and keep the result as a type-safe
  Prometo promise.

## Cancellation

Prometo promises can be cancelled at any point of usage in the code,
using `Prometo.cancel`. The only caveat is that when you cancel a
promise, it doesn't _immediately_ halt whatever is running inside that
promise, but lets it run until its result is used by the next `flatMap`
in the promise chain. At that point, the _next_ promise automatically
turns into a cancelled promise, stopping whatever was about to happen
next from happening.

This also does mean that if you want to cancel a promise chain, you need
to ensure that you keep a reference to the _first_ promise in the chain
and cancel _that._

While not as immediate as something like
[abortable fetch](https://developers.google.com/web/updates/2017/09/abortable-fetch),
in practice this is more general-purpose (it works with any promise, not
just ones returned by `fetch`), and it's enough to prevent errors like
[calling `setState` on an unmounted React component](https://reactjs.org/blog/2015/12/16/ismounted-antipattern.html).
For example:

```reason
let example = fetch("https://example.com");
Prometo.forEach(~f=setState, example);

// later...
Prometo.cancel(example); // this will prevent setState from being called
```

Cancelling a promise too late in the chain won't work:

```reason
let result = "https://example.com"
  |> fetch
  |> Prometo.map(~f=setState);

// later...
Prometo.cancel(result); // won't work, too late, setState has already been called
```

## API docs

Please go to
https://yawaramin.github.io/prometo/Yawaramin__Prometo/Yawaramin__Prometo/
