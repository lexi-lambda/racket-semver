#lang scribble/manual

@(require (for-label typed/racket/base
                     semver))

@title{Semver: Semantic Versioning}

@defmodule[semver]

The @hyperlink["http://semver.org/"]{semver} specification provides a standardized way to represent
version information in a semantic way that can be used for smart and effective dependency management.
By using a standardized, machine-readable format for parsing versions and specifying version ranges,
dependency managers can automatically select patched versions of software without introducing breaking
changes.

The original semver specification imposes very strict rules about what version numbers must mean. In
practice, this is not always practical, but following the guidelines loosely allows for the most
benefit. The @hyperlink["https://github.com/jonathanong/ferver"]{ferver} specification describes a
reinterpretation of the semver rules while still being compatible with semver's format,
so it can still be used with this library.

@; ---------------------------------------------------------------------------------------------------

@defproc[(semver-version? [version String]) Boolean]{

Checks to see if a string represents a valid @hyperlink["http://semver.org/"]{semver} version.

A @deftech{semver version} is composed of three numbers separated by dots, the @italic{major},
@italic{minor}, and @italic{patch} version numbers. When comparing version numbers, these values are
compared in order, and earlier numbers take precedence.

Additionally, a pre-release label may be appended to the version by adding a hyphen, then including an
arbitrary number of dot-delimited groups. The contents of the groups must be limited to ASCII numbers
and letters and hypens (@code{[0-9A-Za-z]}). The groups are used in version comparisons similarly to
the main version numbers. Groups that contain non-numeric characters are compared lexicographically
(ASCII ordering), while those that are strictly numeric are compared as integers.

Furthermore, build metadata may be included at the end of the string. Build metadata takes the same
form as pre-release labels, but it is indicated using a @code{+} instead of a hyphen. However, build
metadata is always ignored in all version operations.

The following are all examples of well-formed version strings:

@(racketblock
  "0.0.0"
  "1.2.3"
  "11.905.67"
  "2.0.3-beta.0"
  "2.0.3-pre-alpha.1.2"
  "2.0.3+b.1056"
  "2.0.3-pre-alpha.1.2+b.1056")

}

@defproc[(semver-comparator? [comparator String]) Boolean]{

Checks to see if a string represents a valid @deftech{semver comparator}. A comparator is a single
version constraint. It may be composed of an optional operation as well as information about the
versions to match. All of the following are valid comparators:

@(racketblock
  "*"
  "1.2.3"
  ">=1.2.3"
  "~1.2.3"
  "~1.2"
  "^0.2.4"
  "<2.0.0")

For information about the semantics of the different comparators, see
@hyperlink["https://github.com/npm/node-semver"]{node-semver}.

}

@defproc[(semver-range? [range String]) Boolean]{

Checks to see if a string represents a valid @deftech{semver range}. A range is simply a string
containing multiple @tech[#:key "semver comparator"]{comparators} separated by whitespace. The
comparators will be matched against a version using AND. If a group of comparators is separated by
@code{||}, then they will be matched using OR.

}

@defproc*[([(semver-version=? [a String] [b String]) Boolean]
           [(semver-version<? [a String] [b String]) Boolean]
           [(semver-version>? [a String] [b String]) Boolean]
           [(semver-version<=? [a String] [b String]) Boolean]
           [(semver-version>=? [a String] [b String]) Boolean])]{

Performs various comparisons on @tech{semver version} strings. Raises an exception if either parameter
does not satisfy @racket[semver-version?].

}

@defproc[(semver-version-within-range? [version String] [range String]) Boolean]{

Determines whether or not a particular @tech[#:key "semver version"]{version} falls within a given
@tech[#:key "semver range"]{range}. If @racket[version] does not satisfy @racket[semver-version?] or
if @racket[range] does not satisfy @racket[semver-range?], then this function raises an exception.

}

@defproc[(semver-maximum-version-within-range [versions (Listof String)]
                                              [range String])
         (Option String)]{

Given a list of @tech[#:key "semver version"]{versions} and a version
@tech[#:key "semver range"]{range}, this finds the maximum version that satisfies the given range. If
no such version exists, it returns @racket[#f].

If any of the @racket[versions] do not satisfy @racket[semver-version?] or if @racket[range] does not
satisfy @racket[semver-range?], then this function raises an exception.

}
