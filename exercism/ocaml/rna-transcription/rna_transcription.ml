type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let swap = function
  | `A -> `U | `C -> `G | `G -> `C | `T -> `A

let to_rna dna =
    List.map swap dna
