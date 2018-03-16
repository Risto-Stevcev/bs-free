open BsEffects;
open BsMocha.Mocha;
let before = BsMocha.Async.before;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsAbstract.Interface;
let (<.) = BsAbstract.Function.Infix.(<.);
let id = BsAbstract.Function.Category.id;

module Fs = {
  open Affect;
  [@bs.val] [@bs.module "fs"]
  external read_file_async :
    ( string, ([@bs.string] [ `hex | `utf8 | `ascii ]), (Js.null(Js.Exn.t), string) => unit
    ) => unit = "readFile";

  [@bs.val] [@bs.module "fs"]
  external write_file_async :
    ( string, string, ([@bs.string] [ `hex | `utf8 | `ascii ]), Js.null(Js.Exn.t) => unit
    ) => unit = "writeFile";

  let read_file : string => affect(string) = path => (error, success) =>
    read_file_async(path, `utf8, (err, content) => {
      err != Js.null ? error(Js.Null.toOption(err)) : success(content);
    });

  let write_file : (string, string) => affect(unit) =
    (path, content) => (error, success) =>
    write_file_async(path, content, `utf8, err => {
      err != Js.null ? error(Js.Null.toOption(err)) : success();
    });
};

type filesystem_f('a)
  = ReadFile(string, string => 'a)
  | WriteFile(string, string, 'a);

module Functor: FUNCTOR with type t('a) = filesystem_f('a) = {
  type t('a) = filesystem_f('a);
  let map = (f, x) => switch x {
  | ReadFile(path, result) => ReadFile(path, f <. result)
  | WriteFile(path, content, a) => WriteFile(path, content, f(a))
  }
};

module Free = Monad.Free(Functor);
module Affect_Apply = BsAbstract.Functions.Apply(Affect.Apply);

let read_file : string => Free.free(string) = path => Free.lift_free(ReadFile(path, id));
let write_file : (string, string) => Free.free(unit) =
  (path, content) => Free.lift_free(WriteFile(path, content, ()));

let program : Free.free(string) = Free.Infix.({
  write_file("sample", "hello") >>=
  (() => read_file("sample")) >>=
  (contents => write_file("sample", contents ++ " world!")) >>=
  (() => read_file("sample"));
});

let interpreter : filesystem_f('a) => Affect.affect('a) = x => switch x {
  | ReadFile(path, result) => Affect.Infix.({
      Fs.read_file(path) >>= contents => Affect.pure(result(contents));
    })
  | WriteFile(path, content, a) => Affect_Apply.Infix.({
      Fs.write_file(path, content) *> Affect.pure(a)
    })
};


describe("Free Monad", () => {
  module Free_Fn = Free.Function(Affect.Monad);
  let file_contents = ref("");

  before(done_ => Affect.Infix.({
    Free_Fn.fold_free(interpreter, program) >>=
    (contents => {
      file_contents := contents;
      Affect.pure(done_())
    }) |>
    Affect.run_affect
  }));

  it("should interpret a free program", () => {
    expect(file_contents^) |> to_be("hello world!");
  })
});
