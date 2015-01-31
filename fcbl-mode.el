;; Copyright (C) 2015  Joakim Jalap

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar fcbl--st 
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\* ". 1" st)
    (modify-syntax-entry ?\> ". 12" st)
    (modify-syntax-entry ?\, "-" st)
    (modify-syntax-entry ?\; "-" st)
    ;; (modify-syntax-entry ?\^ "w" st)
    (modify-syntax-entry ?\$ "w" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\- "w" st)
    (modify-syntax-entry ?\_ "w" st)
    st)
  "Syntax table for COBOL"
  )


(defconst fcbl--keywords '(
       "ACCEPT" "ACCESS" "ADD" "ADVANCING" "AFTER" "ALL" "ALPHABET"
       "ALPHABETIC" "ALPHABETIC-LOWER" "ALPHABETIC-UPPER" "ALPHANUMERIC"
       "ALPHANUMERIC-EDITED" "ALSO" "ALTER" "ALTERNATE" "ANY"
       "ARE" "AREA" "AREAS" "ARITHMETIC" "ASCENDING" "ASSIGN" "AT"
       "AUTHOR" "AUTO" "AUTOMATIC"
       
       "BACKGROUND-COLOR" "BACKGROUND-COLOUR" "BEEP" "BEFORE" "BELL"
       "BINARY" "BLANK" "BLINK" "BLOCK" "BOTTOM" "BY"
       
       "CALL" "CANCEL" "CHAIN" "CHAINING" "CHARACTER" "CHARACTERS" "CLASS"
       "CLOSE" "COBOL"
       "CODE" "CODE-SET" "COLLATING" "COL" "COLUMN" "COLUMNS" "COMMA"
       "COMMIT" "COMMAND-LINE" "COMMON" "COMP" "COMPUTE" "CONTINUE"
       "COPY" "CONFIGURATION" "CURRENCY" "CURSOR" "CONSOLE" "CONVERTING"
       "CONVERSION" "CONSTANT" "COUNT" "CRT" "CURSOR"
       
       "DATA" "DATE" "DATE-WRITTEN" "DATE-COMPILED" "DAY" "DAY-OF-WEEK"
       "DEBUGGING" "DECIMAL-POINT" "DECLARATIVES" "DELETE" "DELIMITED"
       "DELIMITER" "DEPENDING" "DESCENDING" "DISPLAY" "DISC" "DISK"
       "DIVIDE" "DIVISION" "DUPLICATES" "DYNAMIC"
       
       "ELSE" "EMPTY-CHECK" "END" "END-ACCEPT" "END-ADD" "END-CALL"
       "END-COMPUTE" "END-DELETE" "END-DISPLAY" "END-DIVIDE"
       "END-EVALUATE" "END-IF" "END-INVOKE" "END-MULTIPLY" "END-OF-PAGE"
       "END-PERFORM" "END-READ" "END-RECEIVE" "END-RETURN" "END-REWRITE"
       "END-SEARCH" "END-START" "END-STRING" "END-SUBTRACT" "END-UNSTRING"
       "END-WRITE" "ENTER" "ENTRY" "ENVIRONMENT" "EOL" "EOP" "EOS" "EQUAL"
       "EQUALS" "ERASE" "ERROR" "EVALUATE" "EVERY" "EXCEPTION" "EXIT"
       "EXTEND" "EXTERNAL"
       
       "FALSE" "FD" "FILE" "FILE-CONTROL" "FILLER" "FIRST" "FOOTING" "FOR"
       "FOREGROUND-COLOR" "FOREGROUND-COLOUR" "FOREVER" "FREE" "FROM"
       "FULL" "FUNCTION" "FUNCTION-ID"

       "GENERATE" "GIVING" "GLOBAL" "GO" "GOBACK" "GREATER" "GROUP"

       "HIGHLIGHT"

       "IDENTIFICATION" "IF" "IN" "INDEX" "INDEXED" "INITIAL" "INITIALIZE"
       "INITIATE" "INPUT" "INPUT-OUTPUT" "INSPECT" "INTO" "INVALID" "INVOKE"
       "IS"
       
       "JUST" "JUSTIFIED"

       "KEY"

       "LABEL" "LAST" "LEADING" "LEFT" "LEFTLINE" "LEFT-JUSTIFY" "LENGTH"
       "LESS" "LINAGE" "LINAGE-COUNTER" "LINE" "LINE-COUNTER" "LINES"
       "LINKAGE" "LOCALE" "LOCAL-STORAGE" "LOCK"

       "MEMBER" "MEMORY" "MERGE" "MODE" "MOVE" "MULTIPLE" "MULTIPLY"

       "NATIONAL" "NATIVE" "NEGATIVE" "NEXT" "NO" "NO-ECHO" "NOT"
       "NUMBER" "NUMERIC"

       "OBJECT-COMPUTER" "OCCURS" "OF" "OFF" "OMITTED" "ON" "OPEN"
       "OPTIONAL" "OR" "ORDER" "ORGANIZATION" "OUTPUT" "OVERFLOW"

       "PACKED-DECIMAL" "PADDING" "PAGE" "PARAMETER" "PERFORM" "PICTURE"
       "PIC" "POINTER" "POSITIVE" "PRIMARY" "PRINTER" "PRINTING"
       "PROCEDURE" "PROCEED" "PROGRAM" "PROGRAM-ID" "PROGRAM-POINTER"
       "PROMPT"

       "RANDOM" "READ" "READY" "RECORD" "RECORDING" "REDEFINES" "REEL"
       "REFERENCE" "REFERENCES" "RELATIVE" "RELEASE" "REMAINDER" "RENAMES"
       "REPLACE" "REPLACING" "REPORT" "REPOSITORY" "REQUIRED" "RERUN"
       "RESERVE" "RESET" "RETURN" "RETURN-CODE" "RETURNING" "REVERSED"
       "REVERSE-VIDEO" "REWIND" "REWRITE" "RIGHT" "RIGHT-JUSTIFY"
       "ROLLBACK" "ROUNDED" "RUN"

       "SAME" "SCREEN" "SCROLL" "SD" "SEARCH" "SECTION" "SECURE" "SEGMENT-LIMIT"
       "SENTENCE" "SELECT" "SEPARATE" "SEQUENTIAL" "SET" "SHARING" "SIGN"
       "SIZE" "SORT" "SOURCE-COMPUTER" "SPECIAL-NAMES" "START" "STOP" "STRING"
       "SUBTRACT" "SUPPRESS" "SYMBOLIC" "SYNCHRONIZED"

       "TALLYING" "TAPE" "TERMINATE" "THAN" "THEN" "THROUGH" "THRU" "TIME"
       "TIMES" "TO" "TOP" "TRACE" "TRAILING" "TRANSFORM" "TRUE" "TRUNCATION"

       "UNDERLINE" "UNIT" "UNLOCK" "UNSTRING" "UNTIL" "UPDATE" "UPON" "USAGE"
       "USE" "USING"

       "VALUE" "VALUES" "VARYING"

       "WHEN" "WITH" "WORKING-STORAGE" "WRITE"
       )
  "A list of COBOL keywords"
  )


(defconst fcbl--constants '(
       "SPACE"
       "SPACES"
       "SYSTEM-DEFAULT"
       "QUOTE"
       "QUOTES"
       "ZERO"
       "ZEROS"
       "ZEROES"
       )
  "A list of words which can be considered constants in COBOL")


(defconst fcbl--impl-bi-funcs '(
        "ABS"
        "ACOS"
        "ANNUITY"
        "ASIN"
        "ATAN"
        "BYTE-LENGTH"
        "CHAR"
        "COMBINED-DATETIME"
        "CONCATENATE"
        "COS"
        "CURRENCY-SYMBOL"
        "CURRENT-DATE"
        "DATE-OF-INTEGER"
        "DATE-TO-YYYYMMDD"
        "DAY-OF-INTEGER"
        "DAY-TO-YYYYDDD"
        ;"E" ; WTF?
        "EXCEPTION-FILE"
        "EXCEPTION-LOCATION"
        "EXCEPTION-STATEMENT"
        "EXCEPTION-STATUS"
        "EXP"
        "EXP10"
        "FACTORIAL"
        "FRACTIONAL-PART"
        "HIGHEST-ALGEBRAIC"
        "INTEGER"
        "INTEGER-OF-DAY"
        "INTEGER-OF-DATE"
        "INTEGER-PART"
        "LENGTH"
        "LENGTH-AN"
        "LOCALE-COMPARE"
        "LOCALE-DATE"
        "LOCALE-TIME"
        "LOCALE-TIME-FROM-SECS"
        "LOG"
        "LOG10"
        "LOWER-CASE"
        "LOWEST-ALGEBRAIC"
        "MAX"
        "MIDRANGE"
        "MOD"
        "MODULE-CALLER-ID"
        "MODULE-DATE"
        "MODULE-FORMATTED-DATE"
        "MODULE-ID"
        "MODULE-PATH"
        "MODULE-SOURCE"
        "MODULE-TIME"
        "MONETARY-DECIMAL-POINT"
        "MONETARY-THOUSANDS-SEPARATOR"
        "NUMERIC-DECIMAL-POINT"
        "NUMERIC-THOUSANDS-SEPARATOR"
        "NUMVAL"
        "NUMVAL-C"
        "NUMVAL-F"
        "ORD"
        "ORD-MAX"
        "ORD-MIN"
        "PI"
        "PRESENT-VALUE"
        "RANDOM"
        "RANGE"
        "REM"
        "REVERSE"
        "SECONDS-FROM-FORMATTED-TIME"
        "SECONDS-PAST-MIDNIGHT"
        "SIGN"
        "SIN"
        "SQRT"
        "STORED-CHAR-LENGTH"
        "SUBSTITUTE"
        "SUM"
        "TAN"
        "TEST-DATE-YYYYMMDD"
        "TEST-DAY-YYYYDDD"
        "TEST-NUMVAL"
        "TEST-NUMVAL-C"
        "TEST-NUMVAL-F"
        "TRIM"
        "UPPER-CASE"
        "VARIANCE"
        "WHEN-COMPILED"
        "YEAR-TO-YYYY"
        )
  "A list of builtin functions in COBOL, which are implemented in GNU COBOL")


(defconst fcbl--not-impl-bi-funcs '(
        "BOOLEAN-OF-INTEGER"
        "CHAR-NATIONAL"
        "DISPLAY-OF"
        "EXCEPTION-FILE-N"
        "EXCEPTION-LOCATION-N"
        "INTEGER-OF-BOOLEAN"
        "NATIONAL-OF"
        "STANDARD-COMPARE"
        )
  "A list of builtin functions in COBOL which are not yet implemented in GNU
COBOL")


(defconst fcbl--font-lock-keywords
  `(
    (,(regexp-opt fcbl--keywords 'words) . 'font-lock-keyword-face)
    (,(regexp-opt fcbl--constants 'words) . 'font-lock-constant-face)
    (,(regexp-opt fcbl--impl-bi-funcs 'words) . 'font-lock-builtin-face)
    (,(regexp-opt fcbl--not-impl-bi-funcs 'words) . 'font-lock-warning-face
     ))
  "The list passed to font-lock-keywords")


(defconst fcbl--division-rx
  (rx (zero-or-more space) (one-or-more word) (one-or-more space) "division")
  "Regexp to identify a division")


(defconst fcbl--section-rx
  "[[:space:]]*[[:word:]]+[[:space:]]+SECTION\\.[[:space:]]*$"
  "Regexp to identify a section")


(defconst fcbl--verbs
  '("ACCEPT" "ADD" "CALL" "CANCEL" "CLOSE" "COMPUTE" "DELETE" "DISPLAY" "DIVIDE" "EVALUATE"
    "EXIT" "GO" "GOBACK" "IF" "INSPECT" 
    "INVOKE" "INITIALIZE" "MERGE" "MOVE" "MULTIPLY" "OPEN" "PERFORM" "READ" "RECEIVE" "RETURN"
    "REWRITE" "SEARCH" "SET" "SORT" "START" "STRING" "STOP" "SUBTRACT" "UNSTRING" "WRITE")
  "A list of COBOL verbs")


(defconst fcbl--verbs-rx
  (concatenate 'string "[ \t]*" (regexp-opt fcbl--verbs 'words))
  "A regexp to match COBOL verbs")


(defconst fcbl--inline-perform-rx
  "[ \t]*PERFORM[[:space:]]+\\(UNTIL\\|FOREVER\\|VARYING\\|WITH\\|[[:word:]]+[[:space:]]+TIMES\\).*"
  "A regexp to identify an inline perform in COBOL")


(defconst fcbl--block-ender-rx
  "[ \t]*\\(END-[[:word:]]+\\|ELSE\\).*"
  "END-* or ELSE")


(defconst fcbl--paragraph-starter-rx
  "^\\([0-9][0-9][0-9]?\\)?[[:word:]]+\\.[[:space:]]*$"
  "A regexp to identify the start of a paragraph in the procedure
division")

                                        ;TODO: this should be a regexp instead
(defun fcbl--block-starter-p ()
  "A predicate to determine if a line starts a COBOL block.

A 'block' is in this case is a block of statements which should be indented, for exapmple between 'IF' and 'ELSE'"
  (or (looking-at fcbl--inline-perform-rx)
      (looking-at "[ \t]*IF.*")
      (looking-at "[ \t]*ELSE.*")
      (looking-at fcbl--paragraph-starter-rx)))


(defun fcbl--indent-verb ()
  "Function for indenting a line beginning with a COBOL verb.

The way it works is like so: Look at the line above, if that line
is a block-starter (fulfills fcbl--block-starter-p) then indent
one step relative to that line. If the line above starts with a
verb, but not a block starter, indent to the same level as that.
If the line above is a block ender, indent to the same line as
that. If the line above does not start with a verb, deindent two
steps relative to that."
  (indent-line-to
   (save-excursion
     (forward-line -1)
     (while (fcbl--looking-at-blank-line-or-comment-p) (forward-line -1))   
     (cond ((fcbl--block-starter-p) (+ (current-indentation) standard-indent))
           ((looking-at fcbl--verbs-rx) (current-indentation))
           ((looking-at fcbl--block-ender-rx) (current-indentation))
           (t (let ((diff (- (current-indentation) (* 2 standard-indent))))
                (if (> diff 0)
                    diff
                  (current-indentation))))))))


(defun fcbl--looking-at-blank-line-or-comment-p ()
  "True if we're looking at a blank line or a comment"
  (or (looking-at "^[[:space:]]*$")
      (looking-at "^[[:space:]]*\\*>.*")))


(defun fcbl--indent-non-verb ()
  "Function for indenting a line in the procedure
division which does not start with a COBOL verb.

The way it works is like so: Look at the first non blank and non
comment line above the current. If that line starts with a non
verb, indent to that level. If that line starts with a verb,
indent two steps relative to that."
  (indent-line-to
   (save-excursion
     (forward-line -1)
     (while (fcbl--looking-at-blank-line-or-comment-p) (forward-line -1))
     (cond ((looking-at fcbl--verbs-rx)
            (+ (current-indentation) (* 2 standard-indent)))
           (t (current-indentation))))))


(defun fcbl--suffix (str)
  "Get the suffix of block ender str."
   (replace-regexp-in-string "END-" "" str))


(defun fcbl--find-crspdg (dir starter ender target)
  "Finds a corresponding start or end of block.

This function scans for the corresponding starter/ender, skipping
over inner blocks. The direction is determined by the argument
dir, 1 means scan downwards, -1 means scan upwards. The other
three arguments are regexps; starter identifies the block starter
and ender the block ender, while target is the target
starter/ender we're looking for. This will usually be the same as
either starter or ender, the exception is with 'IF'... 'ELSE'...
'END-IF' blocks. If we're looking at such a block and try to find
the corrsponding ender to the 'IF', we do not only want to find
'END-IF's but also 'ELSE's, for example."
  (let ((inner-blocks 0))
    (forward-line dir)
    (while (and (not (and (= 0 inner-blocks)
                          (looking-at target)))
                (not (bobp)) (not (eobp)))
      (cond ((looking-at ender) (incf inner-blocks))
            ((looking-at starter) (decf inner-blocks)))
      (forward-line dir))
    (back-to-indentation)))


(defun fcbl--find-crspdg-starter ()
  "If point is at a block ender, finds the corresponding block starter."
  (back-to-indentation)
  (when (looking-at fcbl--block-ender-rx)
    (let* ((at-else (looking-at "ELSE.*"))
           (at-end-if (looking-at "END-IF.*"))
           (dir -1)
           (starter (if at-else "[ \t]*IF.*"
                      (concat "[ \t]*" (fcbl--suffix (word-at-point)) ".*")))
           (ender (if at-else "[ \t]*END-IF.*"
                    (concat "[ \t]*" (word-at-point) ".*")))
           (target (if at-end-if "[ \t]*\\(IF\\|ELSE\\).*"
                     starter)))
      (fcbl--find-crspdg dir starter ender target))))


(defun fcbl--find-crspdg-ender ()
  "If point is at a block ender, finds the corresponding starter."
  (back-to-indentation)
  (when (fcbl--block-starter-p)
    (let* ((at-else (looking-at "ELSE.*"))
           (at-if (looking-at "IF.*"))
           (dir 1)
           (starter (if at-else "[ \t]*IF.*"
                      (concat "[ \t]*" (word-at-point) ".*")))
           (ender (if at-else "[ \t]*END-IF.*"
                    (concat "[ \t]*END-" (word-at-point) ".*")))
           (target (if at-if "[ \t]*\\(ELSE\\|END-IF\\).*"
                     ender)))
    (fcbl--find-crspdg dir starter ender target))))


(defun fcbl--indent-block-ender ()
  "Indents a block ender by finding the indentation of the
corresponding block starter"
  (when (looking-at fcbl--block-ender-rx)
    (indent-line-to
     (save-excursion
       (fcbl--find-crspdg-starter)
       (current-indentation)))))


(defun fcbl--procdiv-indent-line ()
  "Function for indenting free cobol code in the procedure division"
  (cond ((or (looking-at fcbl--section-rx)
             (looking-at fcbl--paragraph-starter-rx))
         (indent-line-to 0))
        ((or (looking-at fcbl--verbs-rx) (fcbl--looking-at-blank-line-or-comment-p))
         (fcbl--indent-verb))
        ((looking-at fcbl--block-ender-rx) (fcbl--indent-block-ender))
        (t (fcbl--indent-non-verb))))


(defun fcbl--find-division ()
  "Function for finding which division we are in"
  (save-excursion
    (forward-line 1)
    (if (re-search-backward fcbl--division-rx nil t)
        (progn
          (beginning-of-line)
          (cond ((looking-at "procedure") 'procedure)
                ((looking-at "data") 'data)
                ((looking-at "identification") 'identification)
                ((looking-at "environment") 'environment)))
      'nodiv)))


(defun fcbl--datadiv-indent-line ()
  "Function for indenting a line in the data division."
  (cond ((looking-at "[[:space:]]*[[:digit:]][[:digit:]]") (fcbl--indent-wss))
        (t (indent-line-to standard-indent))))


(defun fcbl--indent-wss ()
  "Function for indenting declarations in the working storage section, file section, screen section... Basically all data declarations."
  (indent-line-to
   (save-excursion
     (back-to-indentation)
     (if (not (looking-at "[[:digit:]][[:digit:]]")) 0
       (let ((curr-num (string-to-int (thing-at-point 'word))))
         (if (= curr-num 1) 0
           (progn
             (forward-line -1)
             (back-to-indentation)
             (cond ((= (string-to-int (thing-at-point 'word)) curr-num)
                    (current-indentation))
                   ((< (string-to-int (thing-at-point 'word)) curr-num)
                    (+ standard-indent (current-indentation)))
                   (t 0)))))))))


(defun fcbl--iddiv-indent-line ()
  "Function for indenting a line in the identification division"
  (indent-line-to 0))


(defun fcbl--envdiv-indent-line ()
  "Function for indenting a line in the environment division"
  (indent-line-to 0))


(defun fcbl--find-section ()
  "Function for finding which section we are in"
  (save-excursion
    (forward-line 1)
    (search-backward-regexp fcbl--section-rx)
    (back-to-indentation)
    (thing-at-point 'word)))


(defun fcbl--indent-line ()
  "Function for indenting in Free Cobol mode."
  (interactive)
  (beginning-of-line)
  (cond
   ;; if we're at the begining of the buffer, indent to 0
   ((bobp) (indent-line-to 0))
   ((looking-at fcbl--division-rx) (indent-line-to 0))
   (t (let ((cur-div (fcbl--find-division)))
        (cond ((eq cur-div 'procedure) (fcbl--procdiv-indent-line))
              ((eq cur-div 'data) (fcbl--datadiv-indent-line))
              ((eq cur-div 'environment) (fcbl--envdiv-indent-line))
              ((eq cur-div 'identification) (fcbl--iddiv-indent-line))
              ((eq cur-div 'nodiv) (indent-line-to 0))
              (t (error "not in a division")))))))


(defun fcbl--forward-sexp (&optional n)
  "Moves forward over a sexp, currently only works in the
procedure division."
  (let ((times (if n n 1)))
    (cond ((eq (fcbl--find-division) 'procedure)
           (while (> times 0)
             (fcbl--procdiv-forward-sexp)
             (decf times)))
          (t nil))))


(defun fcbl--procdiv-forward-sexp ()
  "Moves forward over a 'sexp' in the procedure division."
  (cond ((looking-at "^[[:word:]]+\\.[ \t]*$") (fcbl--end-of-defun))
        ((fcbl--block-starter-p) (fcbl--find-crspdg-ender))))


(defun fcbl--beg-of-defun (&optional arg)
  "If point is in procedure division, moves point to beginning of
paragraph"
  (when (eq (fcbl--find-division) 'procedure)
    (re-search-backward fcbl--paragraph-starter-rx nil t arg)))


(defun fcbl--end-of-defun (&optional arg)
  "If point is in procedure division, moves point to beginning of
paragraph"
  (when (eq (fcbl--find-division) 'procedure)
    (forward-line 1)
    (if (re-search-forward fcbl--paragraph-starter-rx nil t arg)
        (backward-word)
      (goto-char (point-max)))
    (forward-line -1)
    (while (fcbl--looking-at-blank-line-or-comment-p) (forward-line -1))))


(define-derived-mode fcbl-mode prog-mode "fcbl"
  "Major mode for editing free format COBOL code."
  :syntax-table fcbl--st
  
  (setq font-lock-defaults '(fcbl--font-lock-keywords nil t))
  (set (make-local-variable 'comment-start) "*>")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'beginning-of-defun-function) 'fcbl--beg-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'fcbl--end-of-defun)
  (set (make-local-variable 'indent-line-function) 'fcbl--indent-line)
  (set (make-local-variable 'forward-sexp-function) 'fcbl--forward-sexp)
)


(provide 'fcbl-mode)

