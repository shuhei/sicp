; Explicit dispatch:
; New type -> Redefine each operation with the new type.
; New operation -> Define the opration that hanldes all existing types.

; Data-directed style:
; New type -> Only implement the new type installer.
; New operation -> Redefine operation installer and define a new generic operation.

; Message-passing style:
; New type -> Only implement the new type maker.
; New operation -> Redefine each type maker with the new operation.

; ---

; New types:
; Message-passing style.

; New operations:
; Explicit dispatch.
