open BsAbstract.Interface;

module Free = (F: FUNCTOR) => {
  type free('a) = FlatMap(F.t(free('a))) | Pure('a);

  module Functor: FUNCTOR with type t('a) = free('a) = {
    type t('a) = free('a);
    let rec map = (f, a) => switch a {
      | Pure(x) => Pure(f(x))
      | FlatMap(x) => FlatMap(F.map(map(f), x))
    };
  };
  module Apply: APPLY with type t('a) = free('a) = {
    include Functor;
    let rec apply = (m, x) => switch m {
      | Pure(f) => map(f, x)
      | FlatMap(f) => FlatMap(F.map(f' => apply(f', x), f))
    };
  };
  module Applicative: APPLICATIVE with type t('a) = free('a) = {
    include Apply;
    let pure = a => Pure(a);
  };
  module Monad: MONAD with type t('a) = free('a) = {
    include Applicative;
    let rec flat_map = (m, f) => switch m {
      | Pure(x) => f(x)
      | FlatMap(x) => FlatMap(F.map(x' => flat_map(x', f), x))
    };
  };
  module Infix = {
    include BsAbstract.Infix.Monad(Monad)
  };

  let lift_free: F.t('a) => free('a) = x => FlatMap(F.map(a => Pure(a), x));

  module Function = (M: MONAD) => {
    let rec iterate: (F.t(M.t('a)) => M.t('a), free('a)) => M.t('a) =
      (f, m) => switch m {
      | Pure(x) => M.pure(x)
      | FlatMap(x) => f(F.map(iterate(f), x))
      };
    let rec fold_free: (F.t('a) => M.t('a), free('b)) => M.t('b) =
      (f, m) => switch m {
      | Pure(x) => M.pure(x)
      | FlatMap(x) => M.flat_map(f(x), fold_free(f))
      };
  };
};

module HoistFree = (F: FUNCTOR, G: FUNCTOR) => {
  module FreeF = Free(F);
  module FreeG = Free(G);
  let rec hoist_free: (F.t('a) => G.t('a), FreeF.free('b)) => FreeG.free('b) =
    (f, m) => switch m {
    | Pure(x) => Pure(x)
    | FlatMap(x) => FlatMap(G.map(hoist_free(f), f(x)))
    };
};
module LowerFree = (M: MONAD) => {
  module FreeM = Free(M);
  module Fn = BsAbstract.Functions.Monad(M);
  let rec lower_free: FreeM.free('a) => M.t('a) =
    m => switch m {
    | Pure(x) => M.pure(x)
    | FlatMap(x) => Fn.flatten(M.map(lower_free, x))
    };
};
