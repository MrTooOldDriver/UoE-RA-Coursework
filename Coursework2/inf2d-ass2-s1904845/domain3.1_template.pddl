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
        (Sharp ?s - item)
        (Key ?k - item)
        (Whetstone ?w - item)
        (Gate ?x - location ?y - location)
        (EmptyHand ?p - player)
    )

    (:action MOVE
        :parameters (?t -player ?x - location ?y - location)
        :precondition (and (BoTheseus ?t) (At ?t ?x) (Connect ?x ?y))
        :effect (and (At ?t ?y) (not (At ?t ?x)))
    )

    (:action PICKUP
        :parameters (?t -player ?o - item ?x - location)
        :precondition (and (BoTheseus ?t) (EmptyHand ?t) (Object ?o) (At ?t ?x) (At ?o ?x))
        :effect (and (Holding ?t ?o) (not (At ?o ?x)) (not(EmptyHand ?t)))
    )

    (:action SLAY
        :parameters (?t -player ?m - player ?x - location ?s - item)
        :precondition (and (BoTheseus ?t) (Minobot ?m) (At ?m ?x) (At ?t ?x) (Holding ?t ?s) (Sword ?s) (Object ?s) (Sharp ?s))
        :effect (and (Defeated ?m))
    )

    (:action OPEN_GATE
        :parameters (?t - player ?x - location ?y - location ?k - item)
        :precondition (and (Gate ?x ?y) (At ?t ?x) (BoTheseus ?t) (Holding ?t ?k) (Object ?k) (Key ?k))
        :effect (and (Connect ?x ?y) (Connect ?y ?x) (not(Gate ?x ?y)) (not(Gate ?y ?x)))
    )

    (:action SHARPEN_SWORD
        :parameters (?t -player ?x - location ?s - item ?w - item)
        :precondition (and (BoTheseus ?t) (At ?t ?x) (At ?w ?x) (Holding ?t ?s) (Sword ?s) (Object ?s) (Whetstone ?w) (Object ?w))
        :effect (and (Sharp ?s))
    )
    
    (:action DROP
        :parameters (?t -player ?x - location ?i - item)
        :precondition (and (BoTheseus ?t) (At ?t ?x) (Holding ?t ?i) (Object ?i) (not(EmptyHand ?t)))
        :effect (and (At ?i ?x) (not(Holding ?t ?i)) (EmptyHand ?t))
    )
    
)
