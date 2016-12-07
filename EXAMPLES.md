The "examples" subdirectory contains a number example statecharts and CSP
scripts that demonstrate possible specifications.

Each example is in its own subdirectory `foo`.  That subdirectory contains a number of files:

- `foo.ea`: The Enterprise Architect statechart.

- `foo.png`: An image of the statechart, useful for users without EA.

- `foo.xml`: The result of exporting the statechart to XML, as described in the
  README

- `foo.csp`: The result of running `specgen` on `foo.xml` (useful for experimenting
   without building specgen locally)

- `runfoo.csp`: A top-level "driver" containing the initial state of the system
  and example specifications.  This is the file to load in FDR.

- `sgbase.csp`: A high-level "library" of useful definitions used by every
  statechart translation.

The remainder of this document describes the examples briefly.  More detailed
explanations of the specifications checked for each example may be found in the
relevant `runfoo.csp` file.

## Phils2, Phils3 and Phils4

These are the 2-, 3-, and 4-philosopher versions of the "dining philosophers"
example described in the paper.  The only property checked is deadlock-freedom,
which does not hold.


## Phils3Lefty

This is the 3-philosopher system, but where one philosopher picks up his forks
in the other order.  In addition to deadlock freedom, we show that there are no
race conditions and how to check the property that "after sitting, no
philosopher stands without eating".


## Phils3LeftyMissingGuard

This is the same as "Phils3Lefty", except that a guard has been left off in a
way that falsifies the property "after sitting, no philosopher stands without
eating", to validate the previous example.


## Phils3LeftyRaceCondition

This is the same as "Phils3Lefty", except that an error has been introduced in
one fork for the purpose of showing how to check for race conditions in variable
writes.
