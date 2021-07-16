# HASH-TABLE-EXT 1.0.0
## What is this?
Tiny extensions for common lisp hash-tables.

## Alternatives and differences.

|     |[cl-ana.hash-table-utils]| [alexandria] | [cl-hash-table-destructuring] | [cl-hash-util] | [hash-set] | hash-table-ext |
| ---               | ---   | ---     | ---    | ---     | ---    | ---    |
| Scope             | wide  | wide    | narrow | narrow  | narrow | narrow |
| Converting        | alist | a/plist |        | a/plist |        |        |
| Listuping         | k/v   | k/v     |        | k       | k/v    |        |
| Getter for nested |       |         |        | \*      |        |        |
| Iteration         | \*    |         |        |         | \*     | \*     |
| Binding           |       |         | \*     | \*      |        | \*     |
| Set ops           |       |         |        |         | \*     | \*     |
| As CLOS object    |       |         |        |         | \*     |        |

[cl-ana.hash-table-utils]: https://github.com/ghollisjr/cl-ana/tree/master/hash-table-utils
[alexandria]: https://gitlab.common-lisp.net/alexandria/alexandria
[cl-hash-table-destructuring]: https://github.com/rplacaman/cl-hash-table-destructuring
[cl-hash-util]: https://github.com/orthecreedence/cl-hash-util
[hash-set]: https://github.com/samebchase/hash-set/

## Usage
For detail, [see spec file.](spec/hash-table-ext.lisp)

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with
* CLISP/2.49
* SBCL/2.1.6
* CCL/1.12
* ECL/20.4.24

## Installation

