Property-based testing stateful systems: a tutorial
================================================================

[![GitHub
CI](https://github.com/stevana/property-based-testing-stateful-systems-tutorial/workflows/CI/badge.svg)](https://github.com/stevana/property-based-testing-stateful-systems-tutorial/actions)
[![Hackage](https://img.shields.io/hackage/v/property-based-testing-stateful-systems-tutorial.svg)](https://hackage.haskell.org/package/property-based-testing-stateful-systems-tutorial)

Property-based testing (PBT), i.e. generating random inputs and checking some
property of the output, of pure programs is an established practice by now. It's
taught in introductory university classes and it's part of test suites in
industry.

Most real world programs are not pure though, they are stateful. While it's
often possible to structure your program in such a way that the impure stuff is
done in `main`, e.g. read the contents of a file, and then passed on to a pure
function, e.g. a parser, it's not always possible. Consider a long-running
program that interacts with the filesystem and with other programs over the
network, e.g. some kind of web service or a distributed database. It's difficult
to split such a program up into doing a little bit of impure stuff at the start,
then hand it over to a pure function (which we can apply PBT on).

Given this it's perhaps a bit surprising that there are relatively few resources
about applying PBT to stateful systems. This repository is an attempt to close
that gap and try to make PBT stateful systems more common.

The goals we'd like to achieve are:

  - Show how to test stateful (i.e. impure/monadic) programs using
    property-based testing;

  - Show how we can do concurrent testing to help uncover problems such as race
    conditions;

  - Show how we can build bigger systems in a modular way by applying the
    property-based testing equivalent of integration and contract tests;

  - Show how to use fault injection and so called simulation testing to
    "end-to-end" test distributed systems;

  - Introduce the reader to related work and open problems in the area along the
    way.

In the interest of brevity, we assume that the reader already has:

  - Enough familiarity with Haskell to be able to read simple programs, for
    example if you can follow along in the *Learn You a Haskell for Great Good!*
    [tutorial](http://learnyouahaskell.com/chapters), then you should be fine;

  - Some experience with property-based testing of non-stateful (i.e. pure)
    programs. For example as explained in the official QuickCheck
    [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) or in the
    following
    [tutorial](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html);

  - Basic knowledge of state machines (i.e.
    [Mealy](https://en.wikipedia.org/wiki/Mealy_machine) / [Moore
    machines](https://en.wikipedia.org/wiki/Moore_machine) and
    [transducers](https://en.wikipedia.org/wiki/Finite-state_transducer)).

Other than that this tutorial is striving to be as self-contained as possibly as
well as accessible to non-Haskell programmers.

Structure
---------

The tutorial is split up into five parts (so far), and each part has the
following structure:

- Motivation: explains why we are doing what we are about to do;
- Plan: how we will do it;
- Code: a concrete implementation of the idea (in case you get stuck when trying
  to implement it yourself);
- Discussion: common questions or objections;
- Exercises: things the authors were to lazy to do, but they know how to;
- Problems: things the authors don't know how to do (yet);
- See also: links to further reading about the topic or related topics;
- Summary: the most important take away.

The parts build upon each other. We start by modelling and testing a simple
counter using a state machine in part 1, we then reuse the same state machine
model to test the counter for thread-safety using linearisability in part 2. In
part 3 we will implement a queue and a web service that uses said queue, the
state machine model for the queue and the real implementation of the queue will
be contract tested to ensure that the model is faithful to the implementation,
subsequently while testing the web service we will use the model in place of the
real queue. In part 4 we introduce fault injection to the queue allowing us to
test how the web service performs when its dependency fails. Finally, in part 5,
we combine all the above ideas in what, sometimes is called simulation testing,
to test a distributed system that uses replicated state machines.

Table of contents
-----------------

1. [State machine testing](./docs/Part01SMTesting.md#readme)
2. [Concurrent state machine testing with
   linearisability](./docs/Part02ConcurrentSMTesting.md#readme)
3. [Integration tests against state machine fakes and consumer-driven contract
   tests for the fakes](./docs/Part03SMContractTesting.md#readme)
4. [Fault-injection](./docs/Part04FaultInjection.md#readme)
5. Simulation testing

Usage
-----

This repository contains literate Haskell code in `src`. If you want to interact
with it, install [`ghcup`](https://www.haskell.org/ghcup/install/) and then type
`cabal repl`. Alternatively, if you are using the
[`nix`](https://nixos.org/download.html) package manager, then running
`nix-shell` in the root directory should give you the right `ghc` version and
all other dependencies you might need.

The literate code is transformed into markdown using
[`pandoc`](https://pandoc.org/) in
[`tools/generate_markdown.sh`](./tools/generate_markdown.sh) and the markdown is
put inside the [`docs`](./docs) directory for easier browsing.

The following is a link to the [first part](./docs/Part01SMTesting.md#readme) of the
generate markdown, at the end it will link to the second part and so on. Or you
can use the table of contents above or the `docs` directory to jump to desired
part straight away.

Contributing
------------

Any feedback, suggestions for improvement or questions are most welcome via the
issue tracker!

See the [`CONTRIBUTING.md`](./.github/CONTRIBUTING.md) file for more detailed
guidelines regarding contributing.

License
-------

See the [`LICENSE`](./LICENSE) file.
