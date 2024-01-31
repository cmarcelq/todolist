#lang racket/gui

(define todo-frame (new frame% [label "Todo List"] [width 600] [height 400]))

(define todo-lists (make-hash))

(define todo-panel (new vertical-panel% [parent todo-frame]))

(define sublist-panel (new vertical-panel% [parent todo-panel]))
(define task-panel (new vertical-panel% [parent todo-panel]))

(define sublist-choice (new choice% [parent sublist-panel] [label ""] [choices '()] [callback (lambda (choice event) (update-todo-list (send sublist-choice get-string (send sublist-choice get-selection))))]))
(define todo-list (new list-box% [parent task-panel] [label ""] [choices '()]))

(define sublist-field (new text-field% [parent sublist-panel] [label "Sublist:"]))
(define todo-field (new text-field% [parent task-panel] [label "Task:"]))

(define (add-sublist)
  ;; Obt�m o valor do campo de texto da sublista
  (define sublist (send sublist-field get-value))
  ;; Se a sublista n�o existir no hash todo-lists
  (unless (hash-has-key? todo-lists sublist)
    ;; Adiciona a sublista ao hash todo-lists
    (hash-set! todo-lists sublist '())
    ;; Adiciona a sublista ao widget de escolha da sublista
    (send sublist-choice append sublist)
    ;; Limpa o campo de texto da sublista
    (send sublist-field set-value "")))

(define (remove-sublist)
  ;; Obt�m a sublista selecionada
  (define sublist (send sublist-choice get-string (send sublist-choice get-selection)))
  ;; Se a sublista for v�lida
  (when sublist
    ;; Remove a sublista do hash todo-lists
    (hash-remove! todo-lists sublist)
    ;; Remove a sublista do widget de escolha da sublista
    (send sublist-choice delete (send sublist-choice find-string sublist))))

(define (add-task)
  ;; Obt�m a sublista selecionada
  (define sublist (send sublist-choice get-string (send sublist-choice get-selection)))
  ;; Se a sublista for v�lida
  (when sublist
    ;; Obt�m as tarefas da sublista
    (define tasks (hash-ref todo-lists sublist))
    ;; Adiciona a nova tarefa �s tarefas da sublista
    (set! tasks (cons (send todo-field get-value) tasks))
    ;; Atualiza as tarefas da sublista no hash todo-lists
    (hash-set! todo-lists sublist tasks)
    ;; Limpa o campo de texto da tarefa
    (send todo-field set-value "")
    ;; Atualiza a lista de tarefas na interface gr�fica
    (update-todo-list sublist)))

(define (remove-task)
  ;; Obt�m a sublista selecionada
  (define sublist (send sublist-choice get-string (send sublist-choice get-selection)))
  ;; Se a sublista for v�lida
  (when sublist
    ;; Obt�m as tarefas da sublista
    (define tasks (hash-ref todo-lists sublist))
    ;; Obt�m a tarefa selecionada
    (define selection (send todo-list get-selection))
    ;; Se a tarefa selecionada for v�lida
    (when selection
      ;; Remove a tarefa selecionada das tarefas da sublista
      (set! tasks (remove (send todo-list get-string selection) tasks))
      ;; Atualiza as tarefas da sublista no hash todo-lists
      (hash-set! todo-lists sublist tasks)
      ;; Atualiza a lista de tarefas na interface gr�fica
      (update-todo-list sublist))))

(define (update-todo-list sublist)
  ;; Limpa a lista de tarefas na interface gr�fica
  (send todo-list clear)
  ;; Obt�m as tarefas da sublista
  (define tasks (hash-ref todo-lists sublist '()))
  ;; Adiciona cada tarefa � lista de tarefas na interface gr�fica
  (for-each (lambda (task) (send todo-list append task)) tasks))

(new button% [parent sublist-panel] [label "Add Sublist"] [callback (lambda (button event) (add-sublist))])
(new button% [parent sublist-panel] [label "Remove Sublist"] [callback (lambda (button event) (remove-sublist))])
(new button% [parent task-panel] [label "Add Task"] [callback (lambda (button event) (add-task))])
(new button% [parent task-panel] [label "Remove Task"] [callback (lambda (button event) (remove-task))])

(send todo-frame show #t)