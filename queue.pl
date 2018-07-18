:- module(queue,[dequeue/3,enqueue/3]).


enqueue(QueueIn, Item, QueueOut) :-
    append(QueueIn, [Item], QueueOut).
dequeue([H|T], H, T).
