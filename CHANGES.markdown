## [1.4.1](https://github.com/diagrams/diagrams-postscript/tree/v1.4) (2018-05-23)

- Allow `base-4.11` and `lens-4.16`
- Add a `Semigroup (Render Postscript V2 Double)` instance

## [1.4](https://github.com/diagrams/diagrams-postscript/tree/v1.4) (2016-10-26)

No significant changes, bumping version to correspond to diagrams 1.4
release.

  - allow `diagrams-core-1.4`
  - allow `diagrams-lib-1.4`

## [1.3.0.7](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.7) (2016-07-20)

- allow `dlist-0.8`

## [1.3.0.6](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.6) (2016-05-01)

- allow `base-4.9`
- allow `data-default-class-0.1`
- test with GHC 8.0.1

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.5...v1.3.0.6)


## [1.3.0.5](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.5) (2016-05-01)

- allow `lens-4.14`

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.4...v1.3.0.5)

## [1.3.0.4](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.4) (2015-11-10)

- allow `semigroups-0.18`

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.3...v1.3.0.4)

## [1.3.0.3](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.3) (2015-09-22)

- allow `lens-4.13` and `semigroups-0.17`

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.2...v1.3.0.3)

## [v1.3.0.2](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.2) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.1...v1.3.0.2)

[v1.3.0.1](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.1) (2015-05-26)
--------------------------------------------------------------------------------------

- allow `lens-4.11`

1.3 (19 April 2015)
-------------------

- Require `diagrams-core-1.3` and `diagrams-lib-1.3`

1.1.0.5 (2 April 2015)
----------------------

 - allow `lens-4.9`
 - allow `vector-space-0.10`

1.1.0.4 (13 Jan 2015)
---------------------

- Allow `vector-space-0.9`
- Allow `lens-4.7`

1.1.0.3 (04 Dec 2014)
---------------------

- Allow `semigroups-0.16`

1.1.0.2 (17 November 2014)
--------------------------

- Allow `lens-4.6`

1.1.0.1 (22 August 2014)
------------------------

    - Allow lens-4.4

1.1 (28 May 2014)
------------------

    - Changes to reflect `Measure` refactoring.
    - Allow `diagrams-core-1.2` and `diagrams-lib-1.2`
    - Allow `semigroups-0.15`
    - Allow `lens-4.2`
    - Allow `mtl-2.2`

1.0.2.4 (10 April 2014)
----------------------

    - Allow `semigroups-0.13`

1.0.2.2 (19 March 2014)
----------------------

  - Allow `lens-4.1`

1.0.2.1 (18 March 2014)
-----------------------

- Allow `dlist-0.7`

1.0.2 (8 March 2014)
--------------------

* **New features**

  - Experimental support for raw CMYK colors.

* **Dependency/version changes**

  - Allow `diagrams-core-1.1` and `diagrams-lib-1.1`.

1.0.1.2 (6 February 2014)
-------------------------

- require diagrams-lib >= 1.0.1 (for Hashable SizeSpec2D instance)

1.0.1.1 (30 January 2014)
-------------------------

- Work around a bug in GHC 7.4.2, which chokes when deriving Generic
  instances for associated data types.

1.0.1 (26 January 2014)
-----------------------

- Add `Hashable (Options Postscript R2)` instance

1.0.0.2 (1 January 2014)
------------------------

- allow semigroups-0.12

1.0.0.1 (30 November 2013)
--------------------------

- Allow dlist-0.6

1.0 (25 November 2013)
----------------------

- Add support for miter limit attribute.
- Use new command-line interface from `diagrams-lib`.
- Export `B` as an alias for `Postscript` token.

0.7.0.2 (27 September 2013)
---------------------------

* allow semigroups-0.11

0.7.0.1 (15 August 2013)
------------------------

* Fix deprecation warning

0.7: 9 August 2013
------------------

First release as an officially supported diagrams backend.

* Simple animation support.
* Upgrade to `monoid-extras-0.3`.
* Allow `base-4.7`.
