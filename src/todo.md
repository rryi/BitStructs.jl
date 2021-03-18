


# recursive BitStruct

## direct access of fields of sub-BS
bs.sym1sym2 or bs.sym1_sym2 if bs.sym1 is a BS having field sym2
(test: faster access?)
Is a BS.generate switch, generates more Val() expressions for _fieldparam
Or give list of pseudo-syms as parameter to generate


## aggregate fields to BS
sym1_sym2 is a pseudo-sym for all fields from sym1 up to sym2
Code generation of all variants is too expensive.
Allow generation for an explicitly given list of pseudo-syms

does it make any sense??
Sub-BS should be enough.



# method parameters

mthods can have Union{BS1,BS2...} as param type, assuming BS1 and BS2 share field names !!