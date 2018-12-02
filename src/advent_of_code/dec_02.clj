(ns advent-of-code.dec-02
  (:require [clojure.string :as str]))

;; Part 1
;; Generate a checksum given the input
;; Checksum = 3s * 2s
(defn twice?
  "Returns true if the string repeats a letter twice"
  [s]
  (some #(= 2 %) (vals (frequencies s))))

(defn thrice?
  "Returns true if the string repeats a letter thrice"
  [s]
  (some #(= 3 %) (vals (frequencies s))))

(def result (apply * (reduce (fn [[twos threes] s]
                               [(if (twice? s) (inc twos) twos)
                                (if (thrice? s) (inc threes) threes)])
                             [0 0] input)))

;; Part 2
;; Which letters are common between the correct IDs
;; Correct Ids are identical except for 1 letter
(defn id-parser
  "Given a pair of IDs return a string with disimilar characters replaced with nil"
  [[a b]]
  (map (fn [x y] (if (= x y) x nil)) a b))

(defn one-off?
  "Given a list return true if there exists only one nil"
  [x]
  (= 1 (get (frequencies x) nil)))

(def pairs (for [a input b input] [a b]))
(def result2 (->> pairs
                  (map id-parser)
                  (reduce #(when (one-off? %2) (reduced %2)))
                  (remove nil?)
                  (str/join "")))

(def input (str/split-lines
            "xdmgyjkpruszabaqwficevtjeo
xdmgybkgwuszlbaqwfichvtneo
xdmgyjkpruszlbcwwfichvtndo
xdmgcjkprusyibaqwfichvtneo
xdmgyjktruszlbwqwficuvtneo
xdmgxjkpruszlbaqyfichvtnvo
xdmgytkpruszlbaqwficuvtnlo
xdmgydkpruszlbaqwfijhvtnjo
xfmgyjkmruszlbaqwfichvtnes
xdmgyrktruszlraqwfichvtneo
xdmgyjkihuszlbaqdfichvtneo
hdmgyjkpruszeiaqwfichvtneo
xdmzyjkpruszlbaqwgichvtnxo
xdmgyjknquszlbpqwfichvtneo
idmgyjrpruszlbtqwfichvtneo
xkmgyjkpruuzlbaqwfichvfneo
xdmgyjkpruszlfaqwficnvtner
xdmgyjkpruszlbpqwficwvteeo
xdmgyjkpwuszlbiqwfhchvtneo
xdmgyjkpruszwbaqwfichrtnbo
xdpgyjkprusblbaqwfgchvtneo
xdmryjkcruszlbaqwfichvtnee
xwmgylkpruszlbaqwfcchvtneo
xdmgyjkpruszflaqwfixhvtneo
xdmgyjkmruszloaqwfichvteeo
xvmgrjkpruszlbaqwfichvsneo
xdmvyjkprusmlbaqwfichvtnes
xdmgyjkpruszlbaqwfichkgbeo
xdmgyikpruxzlbaqwfichvtnei
xdmgyjkprugzlbaqhfichvtveo
xdmgyjkpruszlbaqjaichftneo
xdmzijkpruszlbaqwwichvtneo
xdmgyjkprsszlbaqwfihhvlneo
xdmgyjkprusqlwaqzfichvtneo
ximgyjkpruszlbawwfichvtnen
xsmgyjzpruszlbaqwfichvaneo
xdmgyjkpruszlcaoyfichvtneo
xdmgyjkprusmlbaqvnichvtneo
xdmgyjkvruszmbaqwfichvtueo
xdmgyjppuuszleaqwfichvtneo
xddgyjkprubzlbaqwfichvaneo
xdmgwjkpruszebaswfichvtneo
xdogyjkpruszlblqwfichvdneo
xdkgyjgpruszlbaqwfizhvtneo
xdvgyjkpruszlbdqwfichvtqeo
xdmgyjlpruszlbapwficgvtneo
xdmgyjkpruszlbaqofickvtngo
xdmgyjkprqszliaywfichvtneo
xdqgyjkpruszlbcqwficnvtneo
xdmgdjkpruszlbaqwxichvtseo
xdmgyjkpruczlbaqwfichdtnfo
xdmgyjkpruszluaqwficzvtnjo
xdmgyjkproszlbaqwfacevtneo
xfmgijkpruszlbrqwfichvtneo
odmgyjkpluszlbaqwfichvuneo
xdmgyjkpruszlbaqwwichukneo
xdmgdjkpruszwbaqwfichvtnet
xdmgyjkzrusvlbaqwrichvtneo
xdmgylkprutzlbaqwfichvtnbo
xdmgyjkpruszsbaqwfijtvtneo
xdmgyjkproszlbjqwfichntneo
xdmgyhkpluszlbaqwfichvtnlo
xdmgyjhprushlbaqwfichvtnzo
gdmoyjkpruszlbarwfichvtneo
cdmgyjkpruszlbaqwfcchvtned
xgmgyjkpruszlbaqwfschvtnek
xdmgyjkprusnlzamwfichvtneo
xdmgyjkprgszlbaxwfichvuneo
txmgyjksruszlbaqwfichvtneo
xdmgyjkprusbbbpqwfichvtneo
xdmoyjkpruszlbaqwfighvtxeo
xdmgyjkpruslhbaqwfichptneo
xdmgzjkpruszlbaqwffcmvtneo
xdmgyjkiruszlbaqgficuvtneo
vdbgyjkpruszlbaqwfichvtnek
xdmgyjspruszlbaqwfochvtney
xdmgyjkpruszibaqwfivhvteeo
xdmgyjkpruszfbaqwficbvtgeo
xdmgyjkprystlbaqwxichvtneo
xdmfyjkpryszlxaqwfichvtneo
xdmgyjgpruspybaqwfichvtneo
xdmgyjklruszlbjqwdichvtneo
xdmgyjkzruszltaqwfichvtnek
xdmgqjkpruszlzaqwfichvtneh
xdmgyjhnruszlbaqwficqvtneo
xdmgyjkproszlbaqweichvtnez
xdmgyjkprurzlbaawfichytneo
xdmgyfkpruszlbaqwfschutneo
xdmnyjkpruszlbaawjichvtneo
xdmgyjkpybszlbaqwfichvwneo
xdmgtjkhruszlbaqwfichatneo
xamgyjkprurzlbaqwfichvaneo
xdmgyjkpruszlbaqwgichvtnqv
ndmgyjkpruszlsaqwfuchvtneo
xdmgygkpgusrlbaqwfichvtneo
xdmgyjkpruszfbaqwfichvtnmy
xdmgyjkprupflbaqwfichvjneo
ndmgyjkpruszlbagwfichvtnxo
xdmgyjkpruszlbafwfilhvcneo
xdmgyjkpruszlbaqwfichvjsea
xebgyjkpruszlbaqafichvtneo
xdmkyjdpruszlbaqwfichvtnei
xomgyjkprufzlbaqwfochvtneo
xdmgyjkprfsllbaqwfiihvtneo
xdmyyjkpruszebaqwficmvtneo
xdmnyjkpruczlbarwfichvtneo
xdmgyjkpruszcbaqwbichvtneg
xdmgxjkpluszlbapwfichvtneo
xgrlyjkpruszlbaqwfichvtneo
xdmgyjkpruszlraqwxcchvtneo
xdmhyjupruszlbaqafichvtneo
xdmgnjkpruszlbkqwfjchvtneo
xdmgyjkpruszlwaqwfichvtndg
xdmgfjkpruvqlbaqwfichvtneo
xdmgejkptuszlbdqwfichvtneo
xlmgyjkpruszlnaqwfochvtneo
xdmgcjkpruszlbaqwfiqhvaneo
xdmgyjupruyzlbaywfichvtneo
gdmgyjkpruyzlbaqwficevtneo
xdmgyjkaruazlbapwfichvtneo
xsmiyjkpruszlbaqwfichvtveo
xdmiyjkprukzlbaqwfichvtnea
xdbgmjkxruszlbaqwfichvtneo
xdmgyjkpruskvbaqwfichdtneo
xdmgyjkprusznbaqwficshtneo
xdmgyjkprusrlbaqwfzchetneo
xdmgyrkpruszzbaqwfichvtned
xdmgyjkprusolbacwmichvtneo
xdmgypkpruszlbaqwfichvtmgo
xdmgyjkprumzlbhqwfichttneo
xdmgydkprusglbaqwfichvtnei
xdmuyjkpruszlbpqwfichvtyeo
xdmtymkprusslbaqwfichvtneo
xdmgyjjprkszlbaqwfqchvtneo
xdmgvjdpruszlbaqwfichgtneo
xdtgyjkpruwzlbaqwfjchvtneo
xdmgyjkpruszlbafseichvtneo
xdmgvjkpruszlraawfichvtneo
xdmgyukprgszlbatwfichvtneo
xhmgyjkpruszliaqwnichvtneo
xdmgyjspruszlbwqyfichvtneo
xdmgyjkjruszlqaqwfichvtnvo
xdmgyjkiruszlbnqwfichmtneo
ximgyjkpruszlbaqwfvcevtneo
xdmdyjkpruszlbaqwsithvtneo
ndmgyjkpruszlbaqwfilhatneo
xdmgyjkpruszlbaqwfinhvcnez
xdmgypkpsuszlbajwfichvtneo
xdpgmjkpluszlbaqwfichvtneo
xdmgyjnprupzlbaqwfichvtnel
xbmgyjkprmszlfaqwfichvtneo
xdmgyjkpausllbaqwfichvtseo
xdmgyjkpruszlbaqwfqchttnes
xgmgyjkpruszlbaxwfichvtneb
xdmgyjkpruszabqqwfichvineo
xdmgpjkpquszlbaqwfichvdneo
xdmgyjkeruszlbaqdficbvtneo
xdmaujkpruszlbaqwfichvteeo
xdmgyjkpruszlbaqwrirhvtnev
xdmgyjkpsugzllaqwfichvtneo
xdmgyjkpruszlbaqwfichctnlm
xdmeyjkpruszlbacwfiwhvtneo
xdmgyjkpiuhzlbaqwfijhvtneo
xdmgyjkpruszlbmqhfiohvtneo
xdegyjkpbuszlbbqwfichvtneo
xdmggxkpruszlbaqwfirhvtneo
xdmgojkpruszlbaqvfichvtteo
xdmgyjhtruszlbaqwmichvtneo
rdmgyjkpruszlbaqwfichvthek
xdlgyjqpruszlbaqwfbchvtneo
xdmgyjspriszlbavwfichvtneo
rdkgyjkpruszlbaqwfichvtnuo
tdmgyjkuruszlbaqwfichvtnev
xdmgyjkpxuszlbaqwfkchvtnso
xdegyjkpruszlbbqxfichvtneo
xdmgyjkpruszlbaqwficpvtket
xdmgyjkpruszliaqwfnchvtnec
xdmgyjkpreszlbaqwficdvtdeo
rdmgyjkpruszlbaywfychvtneo
xdmgywkpruszlbaqwficrvtaeo
xdmgyjkpruszlbanwflchvoneo
xdmgyjkpruyzlbaqufychvtneo
symgyjkpruszlbaqwfichvtqeo
xdmgyjkpruszlbaqwfichvbzqo
xzfgyjkpruszlbaqwfichvtveo
udmgyjepruszlbaqwfichbtneo
xhmgyjkpruszlbaqwfjchvtnef
xdhgyjkpruszlbaqaftchvtneo
xdmzyjkjruszlbaqwfichvtnwo
xdmgyjepruszlbaqwffchvtnef
xdmgyjkprurzlbaqwfikhvtneq
xomoyjkpruszkbaqwfichvtneo
xdmgyjkpiuszubaqwfichktneo
xdmgyjkprusdlbaqwhihhvtneo
xdmgyjkpruszlbaqwwirhvxneo
xdmgyjkpruszlbaqwficgitzeo
xdmgyjlpruszlbaqwfichpjneo
xjmgyjkpxuszlbaqwfichatneo
xdmgylkpruszlbaqwfiehvtnez
xdmgbjkpruszmbaqwfihhvtneo
xdmgyjkprubzlwaqwfichvtxeo
xdmgyjhlrustlbaqwfichvtneo
xdmmyjkpruszlbaqwfdchitneo
xdmgyjkpruszlbaqwoichhtbeo
xdzgyjkprvszlcaqwfichvtneo
ndmgyjkpruszlbaqwficavxneo
xdmgyjfpruszxbaqzfichvtneo
xdmgyjkpeuszlbaqzficdvtneo
xdmgyjkpruszlbmqffidhvtneo
xdnvyjkpruszlbafwfichvtneo
xdygyjkpruszlbljwfichvtneo
xdigyjkpruszlbeqwfuchvtneo
xdmgyjkpruszlbpzwfichvteeo
bdmgyjbpruszldaqwfichvtneo
xdmgyjkprrszlbaqmpichvtneo
idmgyjkpruszlbaqyfichvtkeo
xdmgyjkmrqsclbaqwfichvtneo
xdmgyjkpruazlbeqwfichvtxeo
ddmgyjkpruszlbsqwfichotneo
xdmgyqkpruszjbaqwfxchvtneo
xdmnyjkpruozlbaqwfichvtreo
edmgyjkpruszlbuqwwichvtneo
xdmgyjkprmshlbaqwfichctneo
xdmgyjkpruszlbaqwffghotneo
xdmcyjkprfszlbaqnfichvtneo
xdmgyjypruszhbaqwficyvtneo
xdmgyjkprzszlyaqwficmvtneo
xlmgyjkprzszlbaqwficyvtneo
xdmgyjkprutulbaqwfithvtneo
xdygyjkpruszlbpqwfichvpneo
xdmgsjkpoumzlbaqwfichvtneo
xdmgyjkpyuszlbaqdfnchvtneo
xdxgyjkpruszlbaqwfizhvtnjo
xdmgyjkpruszlbaqwfschvkndo
xdmgpjkprnszlcaqwfichvtneo
xhmgyjkpruszlbaqwficgvtnet
xdmgyjkpruswlbaqwfichvtqer
ddmgyjkprcszlbaqwfichqtneo
xdmgyjkpruhhlbaqwpichvtneo
xdmgyjkeraszlbaqwfichvtnso
nomgyjkpruszlbaqwficavxneo
xdmgyjkprdszlbaqwfobhvtneo
xdmgyjkprgszlbaqwfichvtdao
xomgyjspruswlbaqwfichvtneo
xdzgyjkpruszlbaqwficwvpneo
admgejkpruszlbaqwfimhvtneo
xdtgyjkpruszlmaqwfiqhvtneo
xdmgymkprusqlbaqwtichvtneo
xdmgyjkpluszlbaqwfidhvtnea
ztmgyjjpruszlbaqwfichvtneo
"))
