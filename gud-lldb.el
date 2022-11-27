(require 'gud)

;; ======================================================================
;; lldb functions

;; History of argument lists passed to lldb.
(defvar gud-lldb-history nil)

(defvar gud-lldb-marker-regexp
  "^\\(?:\\(?:> .+?(.*?) \\)\\|\\(?: *frame #[0-9]+: .* at \\)\\)\\(.+?\\)\\:\\([0-9]+\\)")

(defvar gud-lldb-marker-regexp-file-group 1)
(defvar gud-lldb-marker-regexp-line-group 2)

(defvar gud-lldb-marker-regexp-start "^-> ")

;; Sample marker lines:
;;     frame #0: 0x0000000100a1299c mysqld`dispatch_command(THD*, COM_DATA const*, enum_server_command) + 552 at /Users/bytedance/code/github/mysql/mysql-server/sql/sql_parse.cc:1652
(defvar gud-lldb-marker-regexp
  "^\\(?:\\(?:> .+?(.*?) \\)\\|\\(?:Frame [0-9]+: \\)\\)\\(.+?\\)\\:\\([0-9]+\\)")

(defvar gud-lldb-marker-regexp-file-group 1)
(defvar gud-lldb-marker-regexp-line-group 2)

(defvar gud-lldb-marker-regexp-start "^> ")

(defvar gud-lldb-marker-acc "")
(make-variable-buffer-local 'gud-lldb-marker-acc)

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-lldb-marker-filter (string)
  (setq gud-lldb-marker-acc (concat gud-lldb-marker-acc string))
  (let ((output ""))
    ;; Process all the complete markers in this chunk.
    (while (string-match gud-lldb-marker-regexp gud-lldb-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (let ((file (match-string gud-lldb-marker-regexp-file-group
                                 gud-lldb-marker-acc))
             (line (string-to-number
                    (match-string gud-lldb-marker-regexp-line-group
                                  gud-lldb-marker-acc))))
         (cons file line))

       ;; Output everything instead of the below
       output (concat output (substring gud-lldb-marker-acc 0 (match-end 0)))
       ;;	  ;; Append any text before the marker to the output we're going
       ;;	  ;; to return - we don't include the marker in this text.
       ;;	  output (concat output
       ;;             (substring gud-lldb-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-lldb-marker-acc (substring gud-lldb-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-lldb-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match gud-lldb-marker-regexp-start gud-lldb-marker-acc)
        (progn
          ;; Everything before the potential marker start can be output.
          (setq output (concat output (substring gud-lldb-marker-acc
                                                 0 (match-beginning 0))))

          ;; Everything after, we save, to combine with later input.
          (setq gud-lldb-marker-acc
                (substring gud-lldb-marker-acc (match-beginning 0))))

      (setq output (concat output gud-lldb-marker-acc)
            gud-lldb-marker-acc ""))

    output))

;; Keeps track of whether the Python lldb_oneshot_break function definition has
;; been exec'ed.
(defvar lldb-oneshot-break-defined nil)

;;;###autoload
(defun lldb (command-line)
  "Run lldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'lldb)))

  (gud-common-init command-line nil 'gud-lldb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'lldb)
  (setq lldb-oneshot-break-defined nil)

  ;; Make lldb dump fullpath instead of basename for a file.
  ;; See also gud-lldb-marker-filter where gud-last-frame is grokked from lldb output.
  (progn
    (gud-call "settings set frame-format frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}\\n")
    (sit-for 1)
    (gud-call "settings set thread-format thread #${thread.index}: tid = ${thread.id}{, ${frame.pc}}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}{, stop reason = ${thread.stop-reason}}\\n")
    (sit-for 1))

  (gud-def gud-listb  "breakpoint list"
    "l"    "List all breakpoints.")
  (gud-def gud-bt     "thread backtrace"
    "b"    "Show stack for the current thread.")
  (gud-def gud-bt-all "thread backtrace all"
    "B"    "Show stacks for all the threads.")

  (gud-def gud-break  "breakpoint set -f %f -l %l"
    "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak
    (progn (gud-call "breakpoint set -f %f -l %l")
      (sit-for 1)
      (if (not lldb-oneshot-break-defined)
        (progn
          ;; The "\\n"'s are required to escape the newline chars
          ;; passed to the lldb process.
          (gud-call (concat "script exec \"def lldb_oneshot_break(frame, bp_loc):\\n"
                      "    target=frame.GetThread().GetProcess().GetTarget()\\n"
                      "    bp=bp_loc.GetBreakpoint()\\n"
                      "    print 'Deleting oneshot breakpoint:', bp\\n"
                      "    target.BreakpointDelete(bp.GetID())\""))
          (sit-for 1)
          ;; Set the flag since Python knows about the function def now.
          (setq lldb-oneshot-break-defined t)))
      (gud-call "breakpoint command add -p %b -o 'lldb_oneshot_break(frame, bp_loc)'"))
    "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-remove "breakpoint clear -f %f -l %l"
    "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "thread step-in"
    "\C-s" "Step one source line with display.")
  (gud-def gud-stepi  "thread step-inst"
    "\C-i" "Step one instruction with display.")
  (gud-def gud-next   "thread step-over"
    "\C-n" "Step one line (skip functions).")
  (gud-def gud-nexti  "thread step-inst-over"
    nil    "Step one instruction (skip functions).")
  (gud-def gud-cont   "process continue"
    "\C-r" "Continue with display.")
  (gud-def gud-finish "thread step-out"
    "\C-f" "Finish executing current function.")
  (gud-def gud-up
    (progn (gud-call "frame select -r 1")
      (sit-for 1))
    "<"    "Up 1 stack frame.")
  (gud-def gud-down
    (progn (gud-call "frame select -r -1")
      (sit-for 1))
    ">"    "Down 1 stack frame.")
  (gud-def gud-print  "expression -- %e"
    "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "expression -- *%e"
    nil    "Evaluate C dereferenced pointer expression at point.")
  (gud-def gud-run    "run"
    "r"    "Run the program.")
  (gud-def gud-stop-subjob    "process kill"
    "s"    "Stop the program.")

  (setq comint-prompt-regexp  "\\(^\\|\n\\)\\*")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'lldb-mode-hook)
  )

(provide 'gud-lldb)
