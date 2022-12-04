(fn process-line [func s]
    (let [(l1* h1* l2* h2*) (string.match s "(%d+)-(%d+),(%d+)-(%d+)")
          [l1 h1 l2 h2] [(tonumber l1*) (tonumber h1*) (tonumber l2*) (tonumber h2*)]
        ]
       (func l1 h1 l2 h2)
    )
)

(fn part-1-func [l1 h1 l2 h2]
    (if 
            ; does 1 contain 2?
            (and (>= l2 l1) (<= h2 h1)) 1
            ; or 2, 1?
            (and (>= l1 l2) (<= h1 h2)) 1
            0
        )
)

(print
    (accumulate [total 0 line (io.lines "input.txt")] 
        (+ total (process-line part-1-func line)))
)