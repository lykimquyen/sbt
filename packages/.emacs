; Ce .emacs peut à priori être utilisé ailleurs qu'à l'ENS, avec un X?Emacs
; pas trop vieux sans trop d'ennuis

; NOTE : « C-x » signifie « contrôle x », et « M-x » signifie « Méta x ».
; « Méta » est, sur les suns les deux touches avec un carré autour de la
; barre d'espace, tandis que sur les PCs, il s'agit de la touche nommée
; « Alt ». Noter que « echap x » est équivalent à « M-x », et est la seule
; solution lorsqu'on fait tourner emacs dans un xterm mal configuré, comme
; ceux de la config conscrit :-(

; MODE D'EMPLOI BREF (pour plus de détails, lisez le manuel (M-? i ->
; Emacs -> Emacs Lisp)) : 
;
; Tout ce qui se trouve entre un point virgule et la fin de la ligne (comme
; ce blabla) est ignoré.
;
; Une expression de la forme « (foo bar gee) », est l'application de la
; fonction « foo » à deux arguments : « bar » et « gee » (qui peuvent eux
; même être formés de la même manière). Ce fichier est une suite de telles
; expressions qui sont évaluées l'une après l'autre. Les constructions du
; langage (« if » par exemple), se présentent sous la forme de fonctions
; (mais il y a une différence : elles n'évaluent pas forcément leurs
; arguments : par exemple, dans « (if foo bar gee) », « foo » est évalué
; d'abord, et en fonction du résultat, c'est « bar » ou « gee » qui sera
; évalué). Pour savoir ce que font les diverses fonctions utilisées, vous
; pouvez taper « M-? f nomdelafonctionquivousinteresse ».
;
; « (setq foo bar) » a pour effet de stocker « bar » dans la variable « foo ».
; Pour savoir à quoi sert une variable, taper « M-? v nomdelavariable ».
;
; Vrai s'écrit « t » et faux « nil » 
; 
; « (lambda (meuh miaou) (blabla) (schtroumpf)) » est une fonction anonyme
; prenant deux argument (« meuh » et « miaou »), et exécutant « (blabla) »,
; puis « (schtroumpf) ».
;
; Un « ' » précédent une expression (par exemple « 'blah ») sert à dire à
; emacs de ne pas essayer d'évaluer l'expression représentée. Cela sert à
; passer une expression non évaluée en argument à une fonction, par exemple
; un _nom_ de variable, ce qui permet par exemple à la fonction de modifier
; le contenu de cette variable.

;load tuareg
;(add-to-list 'load-path "/emacs/tuareg")
;(add-to-list 'load-path "/users/trec/quyen/local/share/emacs/tuareg-pkg")
;(add-to-list 'load-path "/users/trec/quyen/local/share/emacs/tuareg-site-file")
;(add-to-list 'load-path "/users/trec/quyen/local/share/emacs/ocamldebug")
;(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
;(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;(autoload 'camldebug "camldebug" "Run the Caml debugger" t) 

(add-to-list 'load-path "~/tuareg-caml-mode")
(add-to-list 'load-path "~/tuareg-2.0.4")
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;tabbar
(add-to-list 'load-path “~/tabbar-ruler")

(defconst gourous-xemacs (string-match "XEmacs" emacs-version)
  "Vrai si on est sous Xemacs, faux si on est sous Emacs.")

; Un répertoire supplémentaire dans lesquels emacs doit chercher des
; fichiers .el, où vous pouvez installer ce qu'il vous plait.
;(setq load-path (cons (expand-file-name "~/lib/emacs") load-path))
(setq load-path (cons (expand-file-name "~/bin/emacs") load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Quelques points techniques ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Certains terminaux (par exemple xterm avec la config par défaut) envoient
; « C-h » lorsque l'on tape sur backspace. Emacs a eu l'idée géniale
; d'utiliser ce « C-h » pour les fonctions d'aide. On le corrige. Il faut
; utiliser « M-? » à la place de « C-h » pour les fonctions d'aide.  

(define-key global-map [(meta ??)] 'help-command) 
(define-key global-map [(control ?h)] nil)
(keyboard-translate ?\C-h ?\C-?)

; Les blagues avec delete qui fait backspace...
(if gourous-xemacs
    (progn
      (setq delete-key-deletes-forward t)
      (define-key global-map [kp-delete] [delete]))
  (progn 
    (define-key global-map [delete] [?\C-d])
    (define-key global-map [kp-delete] [?\C-d])))


; Pour pouvoir taper des lettres accentuées en mode TTY.
(set-input-mode nil nil 1) 


; La région entre le curseur et la marque n'est highlightée que lorsque
; l'on n'a fait que bouger le curseur après avoir posé la marque.
; Cependant, la marque existe même lorsque la région n'est pas
; highlightée. C'est très pratique, mais il peut arriver que l'on efface tout
; son texte en appuyant sur C-w par mégarde. Dans ce cas, il suffit
; d'utiliser l'undo (C-x u), ou de coller ce qu'on vient de couper (C-y).

(unless gourous-xemacs
  (transient-mark-mode t)
  (setq mark-even-if-inactive t))

; Mais xemacs ne possède pas ce mode : si on ne veut pas que la marque soit
; désactivée, on perd le highlight temporaire, et, beaucoup plus grave, le
; copier-coller n'est plus exporté auprès des autres programmes. C'est ce
; que fait la commande suivante, à vous de voire si vous voulez la
; décommenter. La seule solution valable est d'utiliser Emacs et non
; XEmacs.
;; (if gourous-xemacs 
;;     (setq zmacs-regions nil)
;; )

; Pour empêcher xemacs de tout casser en déplaçant le .emacs.
(if gourous-xemacs (setq load-home-init-file t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cosmétique ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Ces trucs ne servent à rien et bouffent de la place sur l'écran. On les
; laisse par défaut pour faire politiquement correct. Décommentez pour vous
; en débarrasser.
(if gourous-xemacs
    (progn
;;       (set-specifier menubar-visible-p nil)
;;       (set-specifier default-toolbar-visible-p nil)
;;       (set-specifier scrollbar-width 0)
      )
  (progn
;;     (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;     (menu-bar-mode -1) 
;;     (scroll-bar-mode -1)
    )
)

; Le curseur ne clignote pas.
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

; Affiche le numéro de ligne et de colonne dans la barre de titre du buffer.
(line-number-mode 1)
(column-number-mode 1)

; Bip visuel : l'écran flashe au lieu que la machine bippe.
(setq visible-bell t)

; Colorie la parenthèse matchante lorsque le curseur est sur une parenthèse.
(if gourous-xemacs (paren-set-mode 'paren) (show-paren-mode 1))

; Pour avoir des zoulies couleurs.

(require 'font-lock)
(if gourous-xemacs 
    (setq font-lock-auto-fontify t)
  (global-font-lock-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Configuration de divers modes ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Charge un certain nombre de minimodes. Évidemment XEmacs ne connait pas...
(unless gourous-xemacs (require 'generic-x))

; Décommentez si vous voulez auctex.

; (require 'tex-site)

; Le « foo-mode-hook » est exécuté lorsque un buffer passe dans le mode
; « foo » (en général cela se fait automatiquement lors de l'ouverture du
; fichier en fonction de son extension, ce qui se configure à l'aide de la
; variable « auto-mode-alist » (cf un exemple plus loin)). Vous pouvez
; ajouter ce que vous voulez dans les hooks.
;
; Pour rajouter du code dans un hook :
; Avant
; (add-hook 'foo-mode-hook 
;          (lambda()
;           (plouf)
;	    ))
; Après :
; (add-hook 'foo-mode-hook 
;	   (lambda()
;	    (plouf)
;           (mon zoli code à moi)
;           (encore un autre zoli bout de code)
;	    ))
; Il y a principalement deux sortes de choses que vous pouvez ajouter :
; - modifier des variables pour customiser le comportement du mode, je vous
; encourage à regarder la doc des modes (avec « M-? m ») pour voir quelles
; variables peuvent être modifiées.
; - ajouter des raccourcis clavier. Cela se fait avec la fonction
; « local-set-key », qui prend en premier argument la séquence de touches,
; donnés sous une forme du style « [(control ?c) (meta ?d)] » pour
; « C-c M-d », et en deuxième argument la fonction, donnée sous la forme
; « 'nom-de-la-fonction », ou « (lambda () (interactive) (blablabla)) »
; pour une fonction anonyme. Voir le « LaTeX-mode-hook » pour un exemple.



; « text-mode-hook » est appelé aussi par les modes qui dérivent du mode
; texte, en particulier les modes TeX. 
; On y active l'auto-fill, qui sert à passer automatiquement à la ligne au
; bout de « fill-column » caractères. C'est indispensable en mode texte,
; plus discutable en mode programmation puisque l'on y passe à la ligne à
; la main pour des raisons d'indentations. Si vous en voulez aussi dans les
; modes de programmation, vous pouvez ajouter un « (auto-fill-mode 1) »
; dans les hooks des divers modes, ou encore mettre simplement un
; « (setq-default auto-fill-mode t) », qui aura pour effet d'activer
; l'auto-fill partout (dans ce cas, pour le désactiver pour certains modes,
; mettre un « (auto-fill-mode -1) » dans le hook des modes en question).

(add-hook 'text-mode-hook
	  (lambda () 
	    (auto-fill-mode 1)
	    ))

; Les lignes sont coupées au bout de 75 caractères dans les modes où
; l'autofill est activé.
(setq-default fill-column 75)


; Configuration du mode LaTeX.
(add-hook 'latex-mode-hook
          (lambda () 
            (setq tex-command
                  "latex \\\\nonstopmode\\\\input `basename * .tex`")
            (setq tex-dvi-view-command "xdvi")
            ))

; Se placer en mode latex et non tex lors de l'ouverture d'un fichier .tex.
(setq tex-default-mode 'latex-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quelques bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; « global-set-key » fonctionne comme « local-set-key », mais agit sur la
; keymap globale, qui a les deux particularités suivantes :
; - elle est utilisée dans tous les buffers
; - elle a plus faible priorité que les keymaps locales des buffers.

; « compile » permet de lancer une commande, recueille la sortie de la
; commande dans un buffer, puis, à chaque fois que vous tapez « C-x ` »,
; avance dans ce buffer jusqu'à tomber sur un message d'erreur indiquant un
; fichier et une position dans ce fichier (ce que font la plupart des
; compilateurs lorsqu'il y a une erreur dans un programme), et vous amène
; à l'endroit en question. C'est très pratique pour débuguer.
(global-set-key [f9] 'compile)

(global-set-key [(meta ?g)] 'goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Divers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Serveur : le programme emacsclient (gnuclient pour xemacs) appelé sur un
; fichier dit à un emacs lancé au préalable d'ouvrir ce fichier. Vous
; pouvez mettre emacsclient comme éditeur par défaut (variable
; d'environnement EDITOR), ainsi vous pouvez utiliser emacs comme éditeur
; sans avoir à attendre quelques secondes qu'il se lance à chaque fois.
; Taper « C-x # » lorsque vous avez fini.

;me
;(if gourous-xemacs 
;  (progn (setq gnuserv-frame (selected-frame)) (gnuserv-start))
;  (server-start))

; Pour qu'emacs ferme le buffer lorsque l'on C-x #.
;(set (if gourous-xemacs 'gnuserv-temp-file-regexp 'server-temp-file-regexp) "")


; On utilise un dictionnaire français par défaut.
; Pour lancer une vérification d'orthographe : « M-x ispell-buffer ».
(setq ispell-dictionary "francais")

; Lorsque l'on demande de compléter le nom d'un fichier, emacs ignorera les
; fichiers dont le nom se termine par ces suffixes. Vous pouvez y ajouter
; ou en enlever les extensions qui vous chantent.
(setq completion-ignored-extensions 
      (append 
       '(".zo" ".zi" ".cmo" ".cmi" ".cmx" ".aux" ".bbl" ".blg" ".dvi" 
	 ".pdf" ".ps" ".log") 
       completion-ignored-extensions))

; Par défaut, on ouvre un fichier en mode texte.
(setq default-major-mode 'text-mode)

; Vous ne voulez pas utiliser rmail!
(put 'rmail 'disabled t)

; On réactive ces deux commandes.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


; On fait en sorte que tous les fichiers dont le nom contient « makefile »
; ou « Makefile » soient mis en mode make lorsqu'on les ouvre.

(setq auto-mode-alist
      (append (list
		'("[Mm]akefile" . makefile-mode)
		)
	      auto-mode-alist))


; Par défaut, les recherches/remplacements ne tiennent pas compte des
; différences minuscules/majuscules. Ça peut être pénible. Pour désactiver
; dans un buffer : « M-x toggle-case-fold-search ». Pour désactiver
; définitivement, décommentez la ligne suivante :
;; (setq-default case-fold-search nil)

; Par défaut, les fonctions d'indentations utilisent des tabulations, ce
; qui est très pénible. La ligne suivante fait en sorte que seuls des
; espaces soient insérés pour indenter.
(setq-default indent-tabs-mode nil)

; Les deux points suivants sont modifiés dans votre dos par emacs lorsque
; vous utilisez M-x customize.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; BINDINGS
(message "Loading Bindings")

(cua-mode t)

;; iswitch : Changes the behavior of change buffer
;(iswitchb-mode)


;; BINDINGS
(message "Loading Bindings")

(cua-mode t)

;; For Mac addicts, duplicate some usual shortcuts
(global-set-key (kbd "M-c") 'copy-region-as-kill)
;; (global-set-key (kbd "M-v") 'cua-paste)
(global-set-key (kbd "M-z") 'undo)

(global-set-key (kbd "C-S-a") 'mark-whole-buffer) ;; should be C-a but ...
(global-set-key (kbd "C-b") 'iswitchb-buffer)
(global-set-key (kbd "C-S-b") '(lambda () (interactive)
                                 (iswitchb-buffer-other-window)
                                 (other-window -1)))
(global-set-key (kbd "<C-escape>") 'list-buffers)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-/") 'my-comment-region-or-line)
(global-set-key (kbd "C-\\") 'my-uncomment-region-or-line)
(global-set-key (kbd "C-j") 'fill-paragraph)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-S-l") 'recenter)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-o") 'find-file-other-window)
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'save-some-buffers)
(global-set-key (kbd "M-s") 'write-file)
(global-set-key (kbd "C-w") 'kill-buffer)

(global-set-key (kbd "C-M-e") 'insert-euro-symbol)

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-%") 'transpose-chars)

(global-set-key (kbd "C-§") 'my-eol-to-space)

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<C-S-iso-lefttab>") '(lambda () (interactive) (other-window -1)))

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f7>") 'flyspell-buffer)
(global-set-key (kbd "<C-f7>") 'flyspell-mode)
(global-set-key (kbd "<M-f9>") 'compile)
(global-set-key (kbd "<C-f9>") 'recompile)
(global-set-key (kbd "<f9>") 'next-error)
(global-set-key (kbd "<S-f9>") 'previous-error)

(global-set-key (kbd "<f8>") 'my-toggle-menu-and-scrollbar)

(global-set-key (kbd "<f11>") 'pop-tag-mark)
(global-set-key (kbd "<f12>") 'my-goto-tag)
(global-set-key (kbd "<S-f12>") 'my-goto-tag-other-window)
(global-set-key (kbd "<M-f12>") 'find-tag)

(global-set-key (kbd "<S-tab>") 'comint-dynamic-complete-filename)
(global-set-key (kbd "<S-iso-lefttab>") 'comint-dynamic-complete-filename)
(global-set-key (kbd "<backtab>") 'comint-dynamic-complete-filename)

(global-set-key (kbd "C-_") 'split-window-vertically)
(global-set-key (kbd "C-|") 'split-window-horizontally)
(global-set-key (kbd "C-M-|") 'my-split-3)
(global-set-key (kbd "M-SPC") 'dabbrev-expand)
(global-set-key (kbd "C-M-SPC") 'just-one-space)
(global-set-key (kbd "M-e") 'ediff-buffers)
(global-set-key (kbd "M-f") 'grep)
(global-set-key (kbd "C-M-f") 'my-grep)
(global-set-key (kbd "C-.") 'kmacro-end-and-call-macro)

;; FLY-SPELL
(add-hook 'flyspell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-k") 'flyspell-auto-correct-previous-word)
             ))

(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-font-lock-mode 1) ; turn on syntax coloring

(show-paren-mode 1) ; turn on paren match highlighting

(global-linum-mode 1) ; display line numbers in margin. New in Emacs 23

(column-number-mode 1)

;Tabbar

;; setup tabbar (to show tabs)
(add-to-list 'load-path "~/.emacs.d-master/elpa/tabbar-2.0.1")
(require 'tabbar)
(eval-after-load "tabbar"
(tabbar-mode))
