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
        Au - player
        S - item
        K - item
        W - item
        Y1 - item
        Y2 - item
        Y3 - item
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
        (Autoadne Au)
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
        (At Au E)
        (Gate A L)
        (Gate L A)
        (EmptyHand T)
        (EmptyHand Au)
        (Yarn Y1)
        (Object Y1)
        (Yarn Y2)
        (Object Y2)
        (At Y1 E)
        (At Y2 A)
        (Mark E)
        (Moveable Au)
        (Moveable T)
        (= (yarn_remain Y1) 3)
        (= (yarn_remain Y2) 3)
    )

    (:goal (and
        (Defeated M)
    )
  )
)
