
(require 'highlight-chars)

(defface card-suit-diamonds '((t (:foreground "Blue" )))
  "*Face for diamonds"
  :group 'card-suit :group 'faces)

(defface card-suit-clubs '((t (:foreground "Green" )))
  "*Face for clubs"
  :group 'card-suit :group 'faces)

(defface card-suit-spades '((t (:foreground "Black" )))
  "*Face for spades"
  :group 'card-suit :group 'faces)

(defface card-suit-hearts '((t (:foreground "Red" )))
  "*Face for hearts"
  :group 'card-suit :group 'faces)

(hc-highlight-chars '("♦") 'card-suit-diamonds nil t)
(hc-highlight-chars '("♥") 'card-suit-hearts nil t)
(hc-highlight-chars '("♣") 'card-suit-clubs nil t)
(hc-highlight-chars '("♠") 'card-suit-spades nil t)

(when nil
  (hc-highlight-chars '("♦") 'card-suit-diamonds t t)
  (hc-highlight-chars '("♥") 'card-suit-hearts t t)
  (hc-highlight-chars '("♣") 'card-suit-clubs t t)
  (hc-highlight-chars '("♠") 'card-suit-spades t t))

(provide 'card-suit-faces)


;; board: 6 5♥ 2♠ 
;; board: 3♦ 9♥ Q♣ 9♣ 
