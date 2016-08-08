type perf_points = perf_point list
and perf_point = Parsetree.expression

val find : Parsetree.structure -> (perf_points * Parsetree.structure)

val perforate : Parsetree.structure -> float list -> Parsetree.structure
