;;; 08.el --- AOC 2020 - D08 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;;
;;; Commentary:
;;
;;; Code:

(setq lexical-binding t)

(provide '08)
(load-file "./helpers.el")

(rx-define op-rx
  (seq (group (seq (or "acc" "jmp" "nop"))) " " (group (or "+" "-") (* digit))))

(defun parse-op (line)
  "Parse one LINE of machine language."
  (cdr (seq-first (s-match-strings-all (rx op-rx) line))))

(cl-defstruct proc-state ops ip acc visited)

(defun runop (state)
  "Run next operation in the machine with STATE (return next STATE)."
  (let* ((ip (proc-state-ip state))
         (ops (proc-state-ops state))
         (acc (proc-state-acc state))
         (visited (proc-state-visited state))
         (instruction (nth ip ops))
         (opcode (car instruction))
         (operand (string-to-number (cadr instruction))))
    (progn
      (puthash (proc-state-ip state) t visited)
      (pcase opcode
        (`"acc" (make-proc-state :ops ops :ip (+ ip 1) :acc (+ acc operand) :visited visited))
        (`"jmp" (make-proc-state :ops ops :ip (+ ip operand) :acc acc :visited visited))
        (`"nop" (make-proc-state :ops ops :ip (+ ip 1) :acc acc :visited visited))))))

(defun is-runnable-state (state)
  "Get if machine is in runnable STATE."
  (let* ((ops (proc-state-ops state))
         (is-visited (gethash (proc-state-ip state) (proc-state-visited state)))
         (has-instruction (not (null (nth (proc-state-ip state) ops)))))
    (and (not is-visited) has-instruction)))

(defun runapp (instructions)
  "Run INSTRUCTIONS in the FILE."
  (iterate-while 'is-runnable-state
                 'runop
                 (make-proc-state
                  :ops instructions
                  :ip 0
                  :acc 0
                  :visited (make-hash-table :test 'eq))))

(defun runapp-end (instructions)
  "Run INSTRUCTIONS to end or return nill."
  (let ((state (runapp instructions)))
    (if (nth (proc-state-ip state) instructions)
        nil
      state)))

(defun swapop (instructions ip op)
  "Swap one OP in INSTRUCTIONS at IP."
  (progn
    (setf (car (nth ip instructions)) op)
    instructions))

(proc-state-acc (runapp (seq-map 'parse-op (read-lines "08.input"))))

(let* ((instructions (seq-map 'parse-op (read-lines "08.input")))
       (final-state (runapp instructions)))
  (message "Part 1 solution is %s" (proc-state-acc final-state))
  (message "Part 2 solution is %s"
           (seq-map (lambda (ip)
                      (let ((opcode (car (nth ip instructions)))
                            (ops (seq-copy instructions)))
                        (pcase opcode
                          (`"acc" nil)
                          (`"jmp" (runapp-end (swapop ops ip "nop")))
                          (`"nop" (runapp-end (swapop ops ip "jmp"))))))
                    (number-sequence 0 (length instructions)))))

;;; 08.el ends here
;;;
