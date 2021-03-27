(define (problem travelling-21)
    (:domain travelling)
    (:objects
        B - location
        A - location
        L - location
        E - location
        F - location
        D - location
        C - location
        M - player
        T - player
        S - item
    )

    (:init
        (Connect B A)
        (Connect A L)
        (connect E B)
        (connect E F)
        (connect B C)
        (connect C D)
        (Connect A B)
        (Connect L A)
        (connect B E)
        (connect F E)
        (connect C B)
        (connect D C)
        (Minobot M)
        (BoTheseus T)
        (Sword S)
        (Object S)
        (At M L)
        (At T E)
        (At S D)
        (EmptyHand T)
    )

    (:goal (and
	    (Defeated M)
        (At T E)
    )
  )
)
