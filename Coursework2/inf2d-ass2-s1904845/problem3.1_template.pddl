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
        K - item
        W - item
    )

    (:init
	    (Connect B A)
        (connect E B)
        (connect E F)
        (connect B C)
        (connect C D)
        (Connect A B)
        (connect B E)
        (connect F E)
        (connect C B)
        (connect D C)
        (Minobot M)
        (BoTheseus T)
        (Sword S)
        (Object S)
        (Key K)
        (Object K)
        (Whetstone W)
        (Object W)
        (At M L)
        (At T E)
        (At S D)
        (At K F)
        (At W E)
        (Gate A L)
        (Gate L A)
        (EmptyHand T)
    )

    (:goal (and
        (Defeated M)
    )
  )
)
