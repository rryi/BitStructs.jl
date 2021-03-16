
# print, show, dump BitStruct

## short type identification

NamedTople printout is lengthy.
Substitute with type alias defined in macro.

implementation (slow): by reflection? look for constant with value == Type
implementation (medium): build global registration of all types in BitStruct. Could also be used for advanced reflection, e.g. find all BitStruct types having field :fiendmaneToSearch or bitfield type typeToSearch
implementation (fast): add type name symbol to type parameters

## readable type
replace NamedTople printout by macro format: BitStruct{fld::type; ...}