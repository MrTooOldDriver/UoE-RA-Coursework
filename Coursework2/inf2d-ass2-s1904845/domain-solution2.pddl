(define (domain travelling)
    (:requirements :adl )

    (:types
        player
        location
        item
    )

    (:constants
        ;; You should not need to add any additional constants
        Agent - agent
    )

    (:predicates
	    (Sword ?s - item)
        (Object ?o - item)
        (BoTheseus ?t - player)
        (Minobot ?m - player)
        (Connect ?x - location ?y - location)
        (At ?o - object ?l - location)
        (Defeated ?d - player)
        (Holding ?p - player ?o - item)
        (EmptyHand ?p - player)
    )

    (:action MOVE
        :parameters (?t -player ?x - location ?y - location)
        :precondition (and (BoTheseus ?t) (At ?t ?x) (Connect ?x ?y))
        :effect (and (At ?t ?y) (not (At ?t ?x)))
    )

    (:action PICKUP
        :parameters (?t -player ?o - item ?x - location)
        :precondition (and (BoTheseus ?t) (not(Holding ?t ?o)) (Object ?o) (At ?t ?x) (At ?o ?x) (EmptyHand ?t))
        :effect (and (Holding ?t ?o) (not (At ?o ?x)) (not (EmptyHand ?t)))
    )

    (:action SLAY
        :parameters (?t -player ?m - player ?x - location ?s - item)
        :precondition (and (BoTheseus ?t) (Minobot ?m) (At ?m ?x) (At ?t ?x) (Holding ?t ?s) (Sword ?s) (Object ?s))
        :effect (and (Defeated ?m))
    )
)
