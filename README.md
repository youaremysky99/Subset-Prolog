# purelog
## Build
stack init
stack build
## Run
stack exec purelog-exe sample.pl
Enter "quit" to exit.

Enter   ";" to find the next solution,
        "." to terminate searching,
        "query #" to print the # of solutions found,
        "query $" to find the remaining solutions.
## Examples
* ancestor(X, Y).
* append(cons(a, cons(b, nil)), cons(c, cons(d, nil)), R).
* append(cons(a, cons(b, nil)), R, cons(a, cons(b, cons(c, cons(d, nil))))).

Note: ";" is required even for the first solution.
