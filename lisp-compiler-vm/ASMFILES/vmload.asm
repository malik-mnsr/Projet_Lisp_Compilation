((JMP (LABEL ENDVM-LOAD-CODE)) (LABEL VM-LOAD-CODE) (MOVE FP R0)
 (SUB (LIT 2) R0) (LOAD R0 R0) (PUSH R0) (MOVE FP R1) (MOVE SP FP)
 (PUSH (LIT 1)) (MOVE SP R2) (SUB (LIT 2) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL CDR)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0)
 (MOVE FP R0) (SUB (LIT 2) R0) (LOAD R0 R0) (PUSH R0) (MOVE FP R1) (MOVE SP FP)
 (PUSH (LIT 2)) (MOVE SP R2) (SUB (LIT 3) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL SETF)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0)
 (MOVE (VAR INSTR) RO) (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 1))
 (MOVE SP R2) (SUB (LIT 2) R2) (PUSH R2) (PUSH R1) (JSR (LABEL NULL)) (POP R1)
 (POP R2) (MOVE R1 FP) (MOVE R2 SP) (CMP (LIT 0) R0) (JEQ (LABEL ELSE8))
 (MOVE (LIT NIL) R0) (JMP (LABEL ENDIF9)) (LABEL ELSE8) (MOVE (VAR INSTR) RO)
 (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 1)) (MOVE SP R2)
 (SUB (LIT 2) R2) (PUSH R2) (PUSH R1) (JSR (LABEL CAR)) (POP R1) (POP R2)
 (MOVE R1 FP) (MOVE R2 SP) (PUSH R0) (MOVE (VAR LABEL) RO) (PUSH R0)
 (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 1)) (MOVE SP R2) (SUB (LIT 2) R2)
 (PUSH R2) (PUSH R1) (JSR (LABEL QUOTE)) (POP R1) (POP R2) (MOVE R1 FP)
 (MOVE R2 SP) (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2)) (MOVE SP R2)
 (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (JSR (LABEL EQL)) (POP R1) (POP R2)
 (MOVE R1 FP) (MOVE R2 SP) (CMP (LIT 0) R0) (JEQ (LABEL ELSE11))
 (MOVE (VAR CO) RO) (PUSH R0) (MOVE (VAR INSTR) RO) (PUSH R0) (MOVE FP R1)
 (MOVE SP FP) (PUSH (LIT 1)) (MOVE SP R2) (SUB (LIT 2) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL CADR)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0)
 (MOVE FP R0) (SUB (LIT 1) R0) (LOAD R0 R0) (PUSH R0) (MOVE FP R1) (MOVE SP FP)
 (PUSH (LIT 3)) (MOVE SP R2) (SUB (LIT 4) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL VM-EXEC-CHARGER-SYMB)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP)
 (JMP (LABEL ENDIF12)) (LABEL ELSE11) (MOVE (LIT 1) R0) (PUSH R0)
 (MOVE (VAR CO) RO) (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2))
 (MOVE SP R2) (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (MOVE FP R0)
 (SUB (LIT 1) R0) (LOAD R0 R0) (MOVE FP R1) (SUB (LIT 2) R1) (LOAD R1 R1)
 (ADD R1 R0) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0)
 (MOVE (VAR CO) RO) (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2))
 (MOVE SP R2) (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (JSR (LABEL SETF)) (POP R1)
 (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0) (MOVE (VAR CO) RO) (PUSH R0)
 (MOVE (VAR INSTR) RO) (PUSH R0) (MOVE FP R0) (SUB (LIT 1) R0) (LOAD R0 R0)
 (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 3)) (MOVE SP R2)
 (SUB (LIT 4) R2) (PUSH R2) (PUSH R1) (JSR (LABEL VM-EXEC-RESOUDRE-SYMB))
 (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0) (MOVE (VAR CO) RO)
 (PUSH R0) (MOVE FP R0) (SUB (LIT 1) R0) (LOAD R0 R0) (PUSH R0) (MOVE FP R1)
 (MOVE SP FP) (PUSH (LIT 3)) (MOVE SP R2) (SUB (LIT 4) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL SET-MEMOIRE)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP)
 (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2)) (MOVE SP R2)
 (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (JSR (LABEL PROGN)) (POP R1) (POP R2)
 (MOVE R1 FP) (MOVE R2 SP) (JMP (LABEL ENDIF12)) (LABEL ENDIF12)
 (JMP (LABEL ENDIF9)) (LABEL ENDIF9) (PUSH R0) (MOVE FP R1) (MOVE SP FP)
 (PUSH (LIT 0)) (MOVE SP R2) (SUB (LIT 1) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL (INSTR (CAR INSTRUCTIONS)))) (POP R1) (POP R2) (MOVE R1 FP)
 (MOVE R2 SP) (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2)) (MOVE SP R2)
 (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (JSR (LABEL LET)) (POP R1) (POP R2)
 (MOVE R1 FP) (MOVE R2 SP) (PUSH R0) (MOVE (VAR DO) RO) (PUSH R0) (MOVE FP R0)
 (SUB (LIT 2) R0) (LOAD R0 R0) (PUSH R0) (MOVE FP R1) (MOVE SP FP)
 (PUSH (LIT 1)) (MOVE SP R2) (SUB (LIT 2) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL NULL)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0)
 (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 1)) (MOVE SP R2) (SUB (LIT 2) R2)
 (PUSH R2) (PUSH R1) (JSR (LABEL NOT)) (POP R1) (POP R2) (MOVE R1 FP)
 (MOVE R2 SP) (PUSH R0) (MOVE (VAR WHILE) RO) (PUSH R0) (MOVE FP R1)
 (MOVE SP FP) (PUSH (LIT 5)) (MOVE SP R2) (SUB (LIT 6) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL LOOP)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (MOVE FP R1)
 (ADD (LIT 4) R1) (MOVE R1 SP) (RTN) (LABEL ENDVM-LOAD-CODE) (HALT))