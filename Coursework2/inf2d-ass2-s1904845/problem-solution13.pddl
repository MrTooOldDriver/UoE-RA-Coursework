(define (problem travelling-13)
    (:domain travelling)
    (:objects
        B - location
        A - location
        L - location
        M - player
        T - player
        S - item
    )

    (:init
        (Connect B A)
        (Connect A L)
        (Minobot M)
        (BoTheseus T)
        (Sword S)
        (Object S)
        (At M L)
        (At T B)
        (At S A)
    )

    (:goal (and
	    (Defeated M)
    )
  )
)
