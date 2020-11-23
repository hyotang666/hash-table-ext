# HASH-TABLE-EXT 1.0.0
## What is this?
Tiny extensions for common lisp hash-tables.

## Alternatives and differences.

### [cl-ana.hash-table-utils](https://github.com/ghollisjr/cl-ana/tree/master/hash-table-utils)
* One part of the huge data analysis library.
* Having alist converting.
* Having keys and values listuping.
* Lacking functions as set operations.

### [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* Having alist and plist converting.
* Having key and values listuping.
* Lacking iterations.
* Lacking functions as set operations.

### [cl-hash-table-destructuring](https://github.com/rplacaman/cl-hash-table-destructuring)
* Having binds macros.
* Lacking iterations.
* Lacking functions as set operations.

### [cl-hash-util](https://github.com/orthecreedence/cl-hash-util)
* Designed especially for accessing nested hash-tables.
* Having alist and plist converting.
* Having keys listuping.
* Lacking functions as set operations.

### [hash-set](https://github.com/samebchase/hash-set/)
* Designed especially for set operations.
* Introducing `HASH-SET` CLOS object rather than `CL:HASH-TABLE`.

### hash-table-ext
#### Against cl-ana.
I want a tiny specific one.
#### Against alexandria.
In fact, hash-table-ext depends on alexandria.
So hash-table-ext and alexandria are complemented each other.
#### Against cl-hash-table-destructuring.
I need some set operations.
#### Against cl-hash-util.
I need some set operations.
#### Against hash-set.
I want to use `CL:HASH-TABLE` directly.

## Usage

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with

## Installation

