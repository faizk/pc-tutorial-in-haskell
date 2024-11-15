# Parser-combinator tutorial (and a working Scheme interpreter using it)

Starting with the old example from the blue Haskell book,
> A parser of things is a function from strings to lists of pairs
> or things and strings

I decided to write one with my own set of combinators (see `Fun.PC1`) and
then another with `Monad`, `Applicative` and friends (`Fun.PC3`).
(So really, this is yet another tutorial project where I code-golf a bit to
learn some practical applications of Category Theory).

I added a few toy implementations of Scheme interpreters
(`Fun.Scheme1` through `Fun.Scheme3`) with different levels of
completeness - for example, `Scheme1` is more pure and has no aliasing
(the environment just maps names to values),
whereas `Scheme3` has "memory locations" (reference semantics), which makes
it possible to implement `let*` and `letrec` and also things like `define`.

It's nice to keep the simpler and purer `Scheme1` version around at least
for the instructive benefits, while `Scheme3` is obviously more practical to
use. For example, in `Scheme1` the only way to define and use recursive
functions is through the
[Y-Combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator) -
as in [this example](https://github.com/faizk/pc-tutorial-in-haskell/blob/944c0d0d317531f3e0c0c0c05667b744eca72530/test/Spec.hs#L296-L307).

The implementation is more or less the same as [my `jq` version](https://github.com/faizk/fun-with-jq/blob/01d1112385c4c0eac71459ececef8cd2bcd13dcb/lisp2.jq).
There is very simple (read: inefficient) memory allocation and I don't bother with actually collecting the garbage.

(I'm slightly sad to see that the `jq` version is smaller.  I suppose that
perhaps this is telling me I have some way to go before I can say I've
suffieciently upped my Haskell game...)

## Usage

```bash
stack run <<SCHEME
(define (f x) (+ x 1))

(define map (lambda (f l) (if (empty? l) l (cons (f (car l)) (map f (cdr l))))))

(map f '(2 3 4))
SCHEME
```
> should output:
> ```lisp
> (3 4 5)
> ```

Or, you could run it interactively:

```bash
rlwrap -m -M.scm stack run
```

## TODO
- [ ] multi-line input in the REPL
-     (note that the parser itself is OK with
-     input spanning multiple-lines; the REPL is at the moment
-     limited to one-line S-expressions)
- [ ] Just for fun, try implementing garbage collection
-
