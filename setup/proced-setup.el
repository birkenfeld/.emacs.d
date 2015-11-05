;; Proced setup

;; Add better key help with discover
(require 'discover)

(discover-add-context-menu
 :context-menu '(proced
                (description "Process editing")
                (actions
                 ("Basic"
                  ("n" "next" next-line)
                  ("p" "previous" previous-line)
                  ("g" "refresh" revert-buffer)
                  ("k" "kill" proced-send-signal)
                  ("r" "renice" proced-renice))
                 ("Marking"
                  ("m" "mark" proced-mark)
                  ("u" "unmark" proced-unmark)
                  ("M" "mark all" proced-mark-all)
                  ("U" "unmark all" proced-unmark-all)
                  ("C" "mark children" proced-mark-children)
                  ("P" "mark parents" proced-mark-parents))
                 ("Display"
                  ("T" "tree display" proced-toggle-tree)
                  ("s c" "sort by CPU" proced-sort-pcpu)
                  ("s m" "sort by mem" proced-sort-pmem)
                  ("s p" "sort by pid" proced-sort-pid)
                  ("s t" "sort by time" proced-sort-time)
                  ("s u" "sort by user" proced-sort-user))
                 ("Filtering"
                  ("f" "filter" proced-filter-interactive)
                  ("o" "omit marked" proced-omit-processes)
                  ("RET" "refine" proced-refine))
                 ))
 :mode 'proced-mode
 :mode-hook 'proced-mode-hook
 :bind "?")
