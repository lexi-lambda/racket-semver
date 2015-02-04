#lang typed/racket/base

(require racket/list
         racket/string
         racket/match
         typed/alexis/util/comparator)

(provide
 semver-version? semver-comparator? semver-range?
 semver-version-within-range?
 semver-maximum-version-within-range
 (comparison-predicates-out semver-version))

(define semver-regex
  (pregexp
   (string-append
    "^(\\d+)\\.(\\d+)\\.(\\d+)"  ; digits
    "(?:-([0-9A-Za-z\\-]+"       ; suffix start
    "(?:\\.[0-9A-Za-z\\-]+)*))?" ; rest of suffix
    "(?:\\+[0-9A-Za-z\\-]+"      ; build metadata start
    "(?:\\.[0-9A-Za-z\\-]+)*)?$" ; rest of build metadata
    )))

(define comparator-regex
  (pregexp
   (string-append
    "^([=<>~^]|[<>]=)?"          ; prefixes
    "(?:((?:\\d+)|[xX*])"        ; first digit
    "(?:\\.((?:\\d+)|[xX*])"     ; second digit
    "(?:\\.((?:\\d+)|[xX*])"     ; third digit
    "(?:-([0-9A-Za-z\\-]+"       ; suffix start
    "(?:\\.[0-9A-Za-z\\-]+)*))?" ; rest of suffix
    "(?:\\+[0-9A-Za-z\\-]+"      ; build metadata start
    "(?:\\.[0-9A-Za-z\\-]+)*"    ; rest of build metadata
    ")?)?)?)?$"
    )))

(struct SemverVersion ([major : Integer]
                       [minor : Integer]
                       [patch : Integer]
                       [suffix : (Listof (U Integer String))])
  #:transparent)

(struct SemverComparator ([major : Integer]
                          [minor : Integer]
                          [patch : Integer]
                          [suffix : (Listof (U Integer String))]
                          [operation : (U '= '< '> '<= '>=)])
  #:transparent)

(struct SemverRange ([comparators : (Listof (Listof SemverComparator))]) #:transparent)

; Utility function to parse strings to integers.
(: string->integer ((Option String) -> (Option Integer)))
(define (string->integer s)
  (cond
    [(not s) #f]
    [else
     (define i (string->number s))
     (if (exact-integer? i) i #f)]))

; Compares two integers
(: integer-compare (Integer Integer -> (U 0 1 -1)))
(define (integer-compare i1 i2)
  (cond
    [(= i1 i2) 0]
    [(> i1 i2) 1]
    [else     -1]))

; Compares two semver suffixes
(: semver-suffix-compare ((Listof (U Integer String)) (Listof (U Integer String)) -> (U 0 1 -1)))
(define (semver-suffix-compare s1 s2)
  (match (list s1 s2)
    ['(() ()) 0]
    [(list _ '()) 1]
    [(list '() _) -1]
    [(list (list (? string?) ___) (list (? exact-integer?) ___)) 1]
    [(list (list (? exact-integer?) ___) (list (? string?) ___)) -1]
    [(list (list-rest v1 rest1) (list-rest v2 rest2))
     (cond
       [(exact-integer? v1)
        (assert v2 exact-integer?)
        (cond
          [(> v1 v2) 1]
          [(< v1 v2) -1]
          [else (semver-suffix-compare rest1 rest2)])]
       [else
        (assert v2 string?)
        (cond
          [(string>? v1 v2) 1]
          [(string<? v1 v2) -1]
          [else (semver-suffix-compare rest1 rest2)])])
     ]))

; Compares two semver versions
(: semver-version-compare (SemverVersion SemverVersion -> (U 0 1 -1)))
(define (semver-version-compare v1 v2)
  (let ([major1 (SemverVersion-major v1)]
        [major2 (SemverVersion-major v2)]
        [minor1 (SemverVersion-minor v1)]
        [minor2 (SemverVersion-minor v2)]
        [patch1 (SemverVersion-patch v1)]
        [patch2 (SemverVersion-patch v2)]
        [suffix1 (SemverVersion-suffix v1)]
        [suffix2 (SemverVersion-suffix v2)])
    (if (not (= major1 major2)) (integer-compare major1 major2)
        (if (not (= minor1 minor2)) (integer-compare minor1 minor2)
            (if (not (= patch1 patch2)) (integer-compare patch1 patch2)
                (match (list suffix1 suffix2)
                  [(list '() '()) 0]
                  [(list '() _) 1]
                  [(list _ '()) -1]
                  [else (semver-suffix-compare suffix1 suffix2)]))))))

; Compines two semver ranges using OR.
(: semver-range-union (SemverRange SemverRange -> SemverRange))
(define (semver-range-union r1 r2)
  (SemverRange (append (SemverRange-comparators r1)
                       (SemverRange-comparators r2))))

; Checks if a given string is a valid semver version.
(: semver-version? (String -> Boolean))
(define (semver-version? str)
  (regexp-match? semver-regex str))

; Checks if a given string is a valid semver comparator.
(: semver-comparator? (String -> Boolean))
(define (semver-comparator? str)
  (define data (regexp-match comparator-regex str))
  ; fail on cases where a version number is #f but a subsequent number is not
  ; e.g. *.2.3 or 1.*.3
  (define (version-empty? v)
    (or (not v)
        (equal? v "*")
        (equal? v "x")
        (equal? v "X")))
  ; also fail if there's an operation but no version
  (and data
       (not (and (second data) (version-empty? (third data))))
       (or (not (version-empty? (third data))) (version-empty? (fourth data)))
       (or (not (version-empty? (fourth data))) (version-empty? (fifth data)))
       #t))

; Checks if a given string is a valid semver range.
(: semver-range? (String -> Boolean))
(define (semver-range? str)
  (cond
    [(zero? (string-length str)) #t]
    [else
     (define or-clauses (string-split str "||" #:trim? #f))
     (for/and ([clause (in-list or-clauses)]) : Boolean
       (define and-clauses (if (zero? (string-length str)) '("") (string-split clause)))
       (andmap semver-comparator? and-clauses))]))

; Parses a possible semver suffix to a list of the dot-separated components.
(: parse-semver-suffix ((Option String) -> (Listof (U Integer String))))
(define (parse-semver-suffix str)
  (cond
    [(not str) '()]
    [else
     (define split (string-split str "."))
     (for/list ([x (in-list split)])
       (or (string->integer x) x))]))

; Parses a string to a semver.
(: parse-semver-version (String -> SemverVersion))
(define (parse-semver-version str)
  (define data (cast (regexp-match semver-regex str)
                     (Option (List String String String String (Option String)))))
  (cond
    [data
     (SemverVersion (cast (string->integer (second data)) Integer)
                    (cast (string->integer (third data)) Integer)
                    (cast (string->integer (fourth data)) Integer)
                    (parse-semver-suffix (fifth data)))]
    [else (raise-argument-error 'parse-semver-version "semver-version?" str)]))

; Parses a comparator string to a range.
(: parse-semver-comparator (String -> SemverRange))
(define (parse-semver-comparator str)
  (define data (cast (regexp-match comparator-regex str)
                     (Option (List String (Option String) (Option String)
                                   (Option String) (Option String) (Option String)))))
  (cond
    [(and data (semver-comparator? str))
     (define prefix (if (second data)
                        (cast (string->symbol (second data)) (U '= '> '< '>= '<= '~ '^))
                        #f))
     (simplify-semver-comparator (string->integer (third data))
                                 (string->integer (fourth data))
                                 (string->integer (fifth data))
                                 (parse-semver-suffix (sixth data))
                                 prefix)]
    [else (raise-argument-error 'parse-semver-comparator "semver-comparator?" str)]))

; This massive function handles all the possible weird cases of strange semver comparators.
; The resulting ranges are simple and standardized and may be easily compared with versions.
(: simplify-semver-comparator ((Option Integer)
                               (Option Integer)
                               (Option Integer)
                               (Listof (U Integer String))
                               (Option (U '= '> '< '>= '<= '~ '^))
                               -> SemverRange))
(define (simplify-semver-comparator major minor patch suffix operation)
  (case operation
    [(~)
     (cond
       [(not (empty? suffix))
        (SemverRange
         (list
          (list
           (SemverComparator (cast major Integer) (cast minor Integer) (cast patch Integer) suffix '>=)
           (SemverComparator (cast major Integer) (add1 (cast minor Integer)) 0 '() '<))))]
       [patch
        (SemverRange
         (list
          (list
           (SemverComparator (cast major Integer) (cast minor Integer) (cast patch Integer) '() '>=)
           (SemverComparator (cast major Integer) (add1 (cast minor Integer)) 0 '() '<))))]
       [minor
        (SemverRange
         (list
          (list
           (SemverComparator (cast major Integer) (cast minor Integer) 0 '() '>=)
           (SemverComparator (cast major Integer) (add1 (cast minor Integer)) 0 '() '<))))]
       [else 
        (SemverRange
         (list
          (list
           (SemverComparator (cast major Integer) 0 0 '() '>=)
           (SemverComparator (add1 (cast major Integer)) 0 0 '() '<))))])]
    [(^)
     (cond
       [(not minor)
        (SemverRange
         (list (list (SemverComparator (cast major Integer) 0 0 '() '>=)
                     (SemverComparator (add1 (cast major Integer)) 0 0 '() '<))))]
       [(not patch)
        (SemverRange
         (list (list (SemverComparator (cast major Integer) (cast minor Integer) 0 '() '>=)
                     (SemverComparator (cast major Integer) (add1 (cast minor Integer)) 0 '() '<))))]
       [(and (zero? (cast major Integer))
             (zero? minor))
        (SemverRange
         (list (list (SemverComparator 0 0 patch suffix '>=)
                     (SemverComparator 0 0 (add1 patch) '() '<))))]
       [(zero? (cast major Integer))
        (SemverRange
         (list (list (SemverComparator 0 minor patch suffix '>=)
                     (SemverComparator 0 (add1 minor) 0 '() '<))))]
       [else
        (SemverRange
         (list (list (SemverComparator (cast major Integer) minor patch suffix '>=)
                     (SemverComparator (add1 (cast major Integer)) 0 0 '() '<))))])]
    [(#f)
     (cond
       [(not major)
        (SemverRange
         (list (list (SemverComparator 0 0 0 '() '>=))))]
       [(not minor)
        (SemverRange
         (list (list (SemverComparator major 0 0 '() '>=)
                     (SemverComparator (add1 major) 0 0 '() '<))))]
       [(not patch)
        (SemverRange
         (list (list (SemverComparator major minor 0 '() '>=)
                     (SemverComparator major (add1 minor) 0 '() '<))))]
       [else
        (SemverRange
         (list (list (SemverComparator major minor patch suffix '=))))])]
    [else
     (SemverRange
      (list (list
             (SemverComparator (cast major Integer) (or minor 0) (or patch 0) suffix (cast operation (U '= '< '> '<= '>=))))))]))

; Parses a single string describing a semver range into a SemverRange object.
(: parse-semver-range (String -> SemverRange))
(define (parse-semver-range str)
  (define or-clauses (if (zero? (string-length str)) '("") (string-split str "||" #:trim? #f)))
  (define comparators : (Listof SemverRange)
    (for/list ([clause (in-list or-clauses)])
      (define and-clauses (if (zero? (string-length clause)) '("") (string-split clause)))
      (define parsed-ranges (map parse-semver-comparator and-clauses))
      (foldl
       (λ ([r1 : SemverRange] [r2 : SemverRange])
         (SemverRange (list (append (first (SemverRange-comparators r1))
                                    (first (SemverRange-comparators r2))))))
       (SemverRange '(())) parsed-ranges)))
  (foldl semver-range-union (SemverRange '()) comparators))

; Generate semver-version comparison functions
(define-comparison-predicates
  semver-version : String
  semver-version-compare
  #:adapter parse-semver-version : SemverVersion)
; Generate internal semver-version comparison functions
(define-comparison-predicates
  semver-version′ : SemverVersion
  semver-version-compare)

; Checks if a version is within the constraints of a single comparator.
(: semver-version-within-comparator? (SemverVersion SemverComparator -> Boolean))
(define (semver-version-within-comparator? version comparator)
  (define comparator-version (SemverVersion (SemverComparator-major comparator)
                                            (SemverComparator-minor comparator)
                                            (SemverComparator-patch comparator)
                                            (SemverComparator-suffix comparator)))
  (define operation?
    (case (SemverComparator-operation comparator)
      [(=) semver-version′=?]
      [(>) semver-version′>?]
      [(<) semver-version′<?]
      [(>=) semver-version′>=?]
      [(<=) semver-version′<=?]
      [else
       (error 'semver-within-comparator "expected valid comparator operation, got: ~a"
              (SemverComparator-operation comparator))]))
  (operation? version comparator-version))

; Checks if a version is within the constraints of a range.
(: semver-version-within-range? (String String -> Boolean))
(define (semver-version-within-range? version range)
  (define semver (parse-semver-version version))
  (define semrange (parse-semver-range range))
  (for/or ([and-range (in-list (SemverRange-comparators semrange))])
    (for/and ([comparator (in-list and-range)]) : Boolean
      (semver-version-within-comparator? semver comparator))))

; Gets the maximum version that is within the constraints of a range.
; If no such version exists, returns #f.
(: semver-maximum-version-within-range ((Listof String) String -> (Option String)))
(define (semver-maximum-version-within-range versions range)
  (define sorted-versions (sort versions semver-version<?))
  (for/or ([version (in-list (reverse sorted-versions))])
    (if (semver-version-within-range? version range)
        version
        #f)))

#;(module+ test
  (require typed/rackunit)
  (require typed/rackunit/text-ui)
  
  (define tests
    (test-suite
     "Tests"
     
     (test-suite
      "Semver Versions"
      
      (test-case
       "parse semver version without suffix"
       (let ([in "11.5.204"]
             [out (SemverVersion 11 5 204 '())])
         (check-true (semver-version? in))
         (check-equal? (parse-semver-version in) out)))
      
      (test-case
       "parse semver version with suffix"
       (let ([in "11.5.204-pre.0.25a.19"]
             [out (SemverVersion 11 5 204 '("pre" 0 "25a" 19))])
         (check-true (semver-version? in))
         (check-equal? (parse-semver-version in) out)))
      
      (test-case
       "parse semver version with build metadata"
       (let ([in "11.5.204-pre.0.25a.19+2015-01-10.254"]
             [out (SemverVersion 11 5 204 '("pre" 0 "25a" 19))])
         (check-true (semver-version? in))
         (check-equal? (parse-semver-version in) out)))
      
      (test-case
       "fail on malformed semvers"
       (define (check-parse-failure [semver : String])
         (check-false (semver-version? semver) semver)
         (check-exn exn:fail:contract? (λ () (parse-semver-version semver)) semver))
       (check-parse-failure "")
       (check-parse-failure "1")
       (check-parse-failure "1.2")
       (check-parse-failure "1.2.")
       (check-parse-failure "1.2.*")
       (check-parse-failure "x.2.3")
       (check-parse-failure " 1.2.3")
       (check-parse-failure "1.2.3-")
       (check-parse-failure "1.2.3-pre.")
       (check-parse-failure "1.2.3-λ"))
      
      (test-case
       "compare simple versions"
       (check-true (semver-version=? "1.2.3" "1.2.3"))
       (check-false (semver-version>? "1.2.3" "1.2.3"))
       (check-false (semver-version<? "1.2.3" "1.2.3"))
       (check-true (semver-version>=? "1.2.3" "1.2.3"))
       (check-true (semver-version<=? "1.2.3" "1.2.3"))
       (check-true (semver-version>? "1.2.4" "1.2.3"))
       (check-true (semver-version>? "1.3.0" "1.2.3"))
       (check-true (semver-version>? "2.0.0" "1.2.3")))
      
      (test-case
       "compare complex versions"
       (check-true (semver-version=? "1.2.3-pre.1+banana" "1.2.3-pre.1+apple"))
       (check-true (semver-version<? "1.2.3-pre.1" "1.2.3-pre.2"))
       (check-true (semver-version<? "1.2.3-pre" "1.2.3-pre.1"))
       (check-true (semver-version>? "1.2.3-pre.1" "1.2.3-pre"))
       (check-true (semver-version<? "1.2.3-beta" "1.2.3-pre"))
       (check-true (semver-version>? "1.2.3-pre" "1.2.3-beta"))
       (check-true (semver-version<? "1.2.3-pre.2" "1.2.4-pre.1"))
       (check-true (semver-version>? "1.2.3-pre.2" "1.2.3-pre.1"))
       (check-true (semver-version<? "1.2.3-pre.1" "1.2.3-pre.a"))
       (check-true (semver-version>? "1.2.3-pre.a" "1.2.3-pre.1"))))
     
     (test-suite
      "Semver Comparators"
      
      (test-case
       "parse exact semver comparator"
       (let ([in "=11.5.204-pre.0"]
             [out (SemverRange (list (list (SemverComparator 11 5 204 '("pre" 0) '=))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out)))
      
      (test-case
       "parse inexact semver comparators"
       (let ([in "~11.5.7-beta.0"]
             [out (SemverRange (list (list (SemverComparator 11 5 7 '("beta" 0) '>=)
                                           (SemverComparator 11 6 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "~11.5.7"]
             [out (SemverRange (list (list (SemverComparator 11 5 7 '() '>=)
                                           (SemverComparator 11 6 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "~11.5"]
             [out (SemverRange (list (list (SemverComparator 11 5 0 '() '>=)
                                           (SemverComparator 11 6 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "~11"]
             [out (SemverRange (list (list (SemverComparator 11 0 0 '() '>=)
                                           (SemverComparator 12 0 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out)))
      
      (test-case
       "parse carat semver comparators"
       (let ([in "^11.5.7-beta.0"]
             [out (SemverRange (list (list (SemverComparator 11 5 7 '("beta" 0) '>=)
                                           (SemverComparator 12 0 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "^0"]
             [out (SemverRange (list (list (SemverComparator 0 0 0 '() '>=)
                                           (SemverComparator 1 0 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "^0.0"]
             [out (SemverRange (list (list (SemverComparator 0 0 0 '() '>=)
                                           (SemverComparator 0 1 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "^0.2"]
             [out (SemverRange (list (list (SemverComparator 0 2 0 '() '>=)
                                           (SemverComparator 0 3 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "^0.2.5"]
             [out (SemverRange (list (list (SemverComparator 0 2 5 '() '>=)
                                           (SemverComparator 0 3 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "^0.0.5"]
             [out (SemverRange (list (list (SemverComparator 0 0 5 '() '>=)
                                           (SemverComparator 0 0 6 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out)))
      
      (test-case
       "parse semver comparators with implied operation"
       (let ([in "11.5"]
             [out (SemverRange (list (list (SemverComparator 11 5 0 '() '>=)
                                           (SemverComparator 11 6 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out))
       (let ([in "11"]
             [out (SemverRange (list (list (SemverComparator 11 0 0 '() '>=)
                                           (SemverComparator 12 0 0 '() '<))))])
         (check-true (semver-comparator? in))
         (check-equal? (parse-semver-comparator in) out)))
      
      (test-case
       "parse semver comparator with wildcards"
       (let ([ins '("11.5.*" "11.5.x" "11.5.X")]
             [out (SemverRange (list (list (SemverComparator 11 5 0 '() '>=)
                                           (SemverComparator 11 6 0 '() '<))))])
         (for ([in (in-list ins)])
           (check-true (semver-comparator? in))
           (check-equal? (parse-semver-comparator in) out))))
      
      (test-case
       "parse empty semver comparator"
       (let ([ins '("" "*" "x" "X")]
             [out (SemverRange (list (list (SemverComparator 0 0 0 '() '>=))))])
         (for ([in (in-list ins)])
           (check-true (semver-comparator? in))
           (check-equal? (parse-semver-comparator in) out))))
      
      (test-case
       "fail on malformed semver comparators"
       (define (check-parse-failure [comparator : String])
         (check-false (semver-comparator? comparator) comparator)
         (check-exn exn:fail:contract? (λ () (parse-semver-comparator comparator)) comparator))
       (check-parse-failure "a")
       (check-parse-failure "1.")
       (check-parse-failure "1.2.")
       (check-parse-failure "1.2.3.")
       (check-parse-failure "1.*.3")
       (check-parse-failure "x.2.3")
       (check-parse-failure " 1.2.3")
       (check-parse-failure "1.2.3-")
       (check-parse-failure "1.2.3-pre.")
       (check-parse-failure "1.2.3-λ")))
     
     (test-suite
      "Range Tests"
      
      (test-case
       "successfully satisfy range"
       (define (test [version : String] [range : String])
         (check-true (semver-version-within-range? version range)
                     (format "~a within? ~a" version range)))
       (test "1.2.3" "^1.2.3+build")
       (test "1.3.0" "^1.2.3+build")
       (test "1.0.0" "1.0.0")
       (test "1.0.0" "")
       (test "1.2.3" "*")
       (test "1.0.1" ">1.0.0")
       (test "1.99.99" "<=2.0.0")
       (test "2.0.0" "<=2.0.0"))
      
      (test-case
       "find maximum version for range"
       (define (test [versions : (Listof String)] [range : String] [expected : (Option String)])
         (check-equal? (semver-maximum-version-within-range versions range) expected
                       (format "~a maximum ~a -> ~a" versions range expected)))
       (test '("1.2.3" "1.2.4") "1.2" "1.2.4")
       (test '("1.2.3" "1.2.4" "1.2.5" "1.2.6") "~1.2.3" "1.2.6")
       (test '("1.1.0" "1.2.0" "1.2.1" "1.3.0" "2.0.0-b1" "2.0.0-b2" "2.0.0-b3" "2.0.0" "2.1.0")
             "~2.0.0" "2.0.0")))
     
     (test-case
      "Miscellany"
      (check-equal? (integer-compare 5 5) 0))
     
     ))
  
  (run-tests tests))
