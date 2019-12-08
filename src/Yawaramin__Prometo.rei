/** Type-safe, cancelable JavaScript promises for ReasonML. */

/** The 'base' error type of all Prometo promises. */
type error = [ | `Prometo_cancelled];

/** Promise with result type ['a] and polymorphic variant errors of type
    ['e]. At runtime this is just a standard JavaScript promise that is
    never rejected (as long as your code doesn't throw any exceptions.

    We can't tell until runtime whether the promise has been cancelled or
    not, so the error type ['e] always needs to have a 'minimum error' of
    [error]. */
type t('a, 'e) constraint 'e = [> error];

/** [cancel(t)] sets the promise [t] as cancelled. This means that all
    subsequent operations on [t], like [flatMap], [map], [toPromise], etc.
    will be short-circuited to ensure no further work is done.

    In other words, you can cancel a complex promise chain as long as you
    have a reference to the first promise in the chain and call [cancel]
    on it.

    Note that this doesn't include any callbacks registered on the
    promise before it was converted into a Prometo type [t]. In other
    words, any callbacks registered using [Js.Promise.then_] will {i not}
    be cancelled. */
[@bs.set]
external cancel: (t('a, 'e), [@bs.as {json|true|json}] _) => unit =
  "_prometo_cancelled";

/** [flatMap(~f, t)] runs the function [f] on the completion result of
    promise [t], unless [t] is in an error state or is cancelled. If
    successful it returns the result of [f]. */
let flatMap: (~f: 'a => t('b, 'e), t('a, 'e)) => t('b, 'e);

/** [forEach(~f, t)] calls [f] with the successful result of [t] and
    discards the result. If [t] is errored or cancelled, is a no-op. */
let forEach: (~f: 'a => 'b, t('a, 'e)) => unit;

/** [make(a)] returns a promise which contains the successful result [a]. */
let make: 'a => t('a, error);

/** [map(~f, t)] returns a promise which has the result of calling [f] on
    the input promise [t]'s result. If [t] is errored or cancelled,
    returns it directly. */
let map: (~f: 'a => 'b, t('a, 'e)) => t('b, 'e);

/** [fromPromise(promise)] converts a JavaScript promise into a Prometo
    promise (this means ensuring that the promise is not rejected). */
let fromPromise:
  Js.Promise.t('a) => t('a, [> | `Prometo_error(Js.Promise.error)]);

/** [toPromise(t)] converts the Prometo promise [t] into a standard
    JavaScript promise. This means 'unwrapping' the contained result
    value and rejecting the promise if the result was an error or if [t]
    was cancelled. */
let toPromise:
  t('a, [> error | `Prometo_error(Js.Promise.error)]) => Js.Promise.t('a);

/** Operations to handle promises containing errors. */
module Error: {
  /** [forEach(~f, t)] calls [f] with the error result of [t] and
     discards the result. If [t] is succeeded or cancelled, is a no-op. */
  let forEach: (~f: 'e1 => [> error], t('a, 'e1)) => unit;

  /** [flatMap(~f, t)] converts an errored promise [t] into a new promise
      using the function [f]. The new promise may be succeeded or errored,
      unless [t] is cancelled, in which case the new promise is also
      cancelled.

      If [t] is a succeeded promise, returns [t] directly. */
  let flatMap: (~f: 'e1 => t('a2, 'e2), t('a1, 'e1)) => t('a2, 'e2);

  /** [make(e)] returns a promise which contains the error result [e]. */
  let make: 'e => t('a, 'e);

  /** [map(~f, t)] returns a promise which has the error result of
      calling [f] on the input promise [t]'s error result. If [t] is
      succeeded or cancelled, returns it directly. */
  let map: (~f: 'e1 => 'e2, t('a, 'e1)) => t('a, 'e2);

  /** [recover(~f, t)] returns a promise with a success result obtained
      by calling [f] on the failure result of [t]. This is unless [t] has
      a successful result, or is cancelled, in which case this is a no-op.
      In other words: it's not possible (by design) to recover from a
      cancelled promise. */
  let recover: (~f: 'e => 'a2, t('a1, 'e)) => t('a2, error);
};
