(fn process-line [func s]
    (let [(l1* h1* l2* h2*) (string.match s "(%d+)-(%d+),(%d+)-(%d+)")
          [l1 h1 l2 h2] [(tonumber l1*) (tonumber h1*) (tonumber l2*) (tonumber h2*)]
        ]
       (func l1 h1 l2 h2)
    )
)

(fn part-2-func [l1 h1 l2 h2]
    (if 
            (> l2 h1) 0
            (> l1 h2) 0
            1
        )
)

(print
    (accumulate [total 0 line (io.lines "input.txt")] 
        (+ total (process-line part-2-func line)))
)