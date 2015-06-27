;; Sets up haskell stuff

;; Cyclic indent, haddock, types in minibuffer
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq inferior-haskell-find-project-root nil)

;; Much better than plain cabal, if cabal-install is new enough
(setq haskell-process-type 'cabal-repl)

;; Evil in an interactive buffer ... no thanks
(add-hook 'haskell-interactive-mode-hook 'turn-off-evil-mode)

