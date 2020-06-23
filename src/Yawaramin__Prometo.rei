[@text {|Type-safe, cancelable JavaScript promises for ReasonML.|}]

/** The 'base' error type of all Prometo promises. If a promise is in a
    failed state, it is always possible for the failure to be from a
    caught exception, or because the promise was cancelled. */
type error = [ | `Prometo_error(Js.Promise.error) | `Prometo_cancelled];

/** Promise with result type ['a] and polymorphic variant errors of type
    ['e]. At runtime this is just a standard JavaScript promise that is
    never rejected (as long as your code doesn't throw any exceptions.

    We can't tell until runtime whether the promise has been cancelled or
    not, so the error type ['e] always needs to have a 'minimum error' of
    [error]. */
type t(+_, 'e) constraint 'e = [> error];

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
let cancel: t(_, _) => unit;

/** [flatMap(~f, t)] runs the function [f] on the completion result of
    promise [t], unless [t] is in an error state or is cancelled. If
    successful it returns the result of [f]. If [f] throws an exception
    it catches and safely returns a promise in an error state. */
let flatMap: (~f: 'a => t('b, 'e), t('a, 'e)) => t('b, 'e);

/** [forEach(~f, t)] calls [f] with the successful result of [t] and
    discards the result. If [t] is errored or cancelled, is a no-op. */
let forEach: (~f: 'a => unit, t('a, _)) => unit;

/** [fromArray(array)] chains the given [array] of promises in sequence,
    returning a single promise containing the array of results of the
    input promises. */
let fromArray: array(t('a, 'e)) => t(array('a), 'e);

/** [fromPromise(promise)] converts a JavaScript promise into a Prometo
    promise (this means ensuring that the promise is not rejected). */
let fromPromise: Js.Promise.t('a) => t('a, _);

/** [make(a)] returns a promise which contains the successful result [a]. */
let make: 'a => t('a, error);

/** [map(~f, t)] returns a promise which has the result of calling [f] on
    the input promise [t]'s result. If [t] is errored or cancelled,
    returns it directly. */
let map: (~f: 'a => 'b, t('a, 'e)) => t('b, 'e);

/** [thenPromise(~f, t)] runs [f] on the result of the promise [t],
    safely converting its resulting JavaScript Promise into a Prometo
    promise. I.e., it lifts up JavaScript promise-chain functions into
    Prometo promise-chain functions. */
let thenPromise: (~f: 'a => Js.Promise.t('b), t('a, 'e)) => t('b, 'e);

/** [toPromise(t)] converts the Prometo promise [t] into a standard
    JavaScript promise. This means 'unwrapping' the contained result
    value and rejecting the promise if the result was an error or if [t]
    was cancelled. */
let toPromise:
  t('a, [> error | `Prometo_error(Js.Promise.error)]) => Js.Promise.t('a);

[@text {|{2 Joining promises together}

    The following functions join multiple promises together into a single
    promise containing the results of all the individual promises.|}]

let zip2: (t('a1, 'e), t('a2, 'e)) => t(('a1, 'a2), 'e);
let zip3: (t('a1, 'e), t('a2, 'e), t('a3, 'e)) => t(('a1, 'a2, 'a3), 'e);
let zip4:
  (t('a1, 'e), t('a2, 'e), t('a3, 'e), t('a4, 'e)) =>
  t(('a1, 'a2, 'a3, 'a4), 'e);
let zip5:
  (t('a1, 'e), t('a2, 'e), t('a3, 'e), t('a4, 'e), t('a5, 'e)) =>
  t(('a1, 'a2, 'a3, 'a4, 'a5), 'e);
let zip6:
  (
    t('a1, 'e),
    t('a2, 'e),
    t('a3, 'e),
    t('a4, 'e),
    t('a5, 'e),
    t('a6, 'e)
  ) =>
  t(('a1, 'a2, 'a3, 'a4, 'a5, 'a6), 'e);

/** Operators that may be handy when processing lots of promises. */
module Infix: {
  /** The same as [flatMap]. */
  let (>>=): (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e);

  /** The same as [map]. */
  let (>|=): (t('a, 'e), 'a => 'b) => t('b, 'e);
};

/** Operations to handle promises containing errors. */
module Error: {
  /** [forEach(~f, t)] calls [f] with the error result of [t] and
     discards the result. If [t] is succeeded or cancelled, is a no-op. */
  let forEach: (~f: 'e => unit, t(_, 'e)) => unit;

  /** [flatMap(~f, t)] converts an errored promise [t] into a new promise
      using the function [f]. The new promise may be succeeded or errored,
      unless [t] is cancelled, in which case the new promise is also
      cancelled.

      If [f] throws an exception, safely catches it and returns a promise
      in an error state.

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
  let recover: (~f: 'e => 'a2, t('a1, 'e)) => t('a2, _);
};
