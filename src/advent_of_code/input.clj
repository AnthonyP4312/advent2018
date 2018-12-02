(ns advent-of-code.input
  (:require [clojure.string :as str]))

(def day2 (str/split-lines
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

(def day1
  "-4
+7
+3
+1
-9
-14
+18
-7
-5
-18
+11
-8
+17
-16
-19
+14
+11
-8
+14
+22
+13
+14
-18
+8
-16
+10
-12
+9
-19
-12
-6
+10
+2
-14
+18
+17
+11
-5
+6
+9
+16
-3
+12
+5
+15
+7
+2
-5
-13
+7
+19
+10
-2
+3
-5
-7
-11
+14
+13
+3
+11
+15
-19
-1
-5
+15
+14
-16
+8
+9
-11
-7
+15
+4
+7
+11
-2
+17
-8
-14
+3
-4
+18
+1
+6
-5
+17
+13
-14
-15
-9
+16
+14
+12
-14
-14
+4
-19
+11
+5
+7
+1
+13
-7
+19
+12
+10
+13
-3
+6
-17
+13
-8
+16
+4
-15
+14
-1
+15
-19
+15
-19
+17
-6
+11
-10
+9
+17
+6
+15
-18
+2
-8
+11
-6
-7
+9
+16
-13
-18
-1
+5
-16
+9
-6
+11
-16
+3
-9
+18
+5
+7
+15
+12
+19
-17
-4
-15
-13
+5
-11
+25
+14
+5
+5
+4
-2
-4
-14
+7
+16
+6
+11
-8
+13
+8
+3
-12
+8
+7
-5
+14
+5
+8
-3
-14
-14
-7
-18
-12
+18
+4
-14
-17
-17
+12
-9
+3
-18
+27
+26
-9
-7
+12
+9
+6
+7
+1
-13
+4
-8
-3
-12
+21
+13
+6
+7
+9
+18
-17
+4
+10
-11
-17
-16
+18
+9
-2
+3
-13
-9
+3
-10
-15
+11
+24
+2
+16
+11
+18
+14
-5
-3
+18
+4
+4
+5
-2
-10
+8
+19
+4
+14
+8
+1
-14
-7
+15
-4
-5
+20
+15
+2
-14
-12
-16
+4
-9
+18
+16
+7
+7
+10
+17
-18
+3
+10
-11
-21
-9
-2
+12
-17
+13
+19
-18
-4
-19
+10
-11
-18
-4
+6
+1
+2
-4
-28
-28
-19
+13
+18
-15
-6
+5
+9
-26
-6
+19
-6
+1
-11
-11
+25
-26
+19
+10
+2
+9
-22
-66
-8
+1
-14
-2
-5
+19
-55
+6
-19
-4
-13
+21
-13
-3
-10
-7
+15
-5
-4
-10
+5
-11
-3
+16
+6
+10
+3
-1
-5
-21
-6
+9
-4
-6
-17
-10
+14
-11
+17
+16
-19
+6
+1
-2
+8
-15
+17
-14
-16
+7
-5
-24
-7
-20
+5
-4
+11
-1
+12
-9
-11
-9
-9
-11
-3
-15
-21
-12
+21
+3
+12
+2
+2
+21
-9
+20
+4
+11
-1
+4
-20
+4
+19
+12
+4
-15
+14
+11
+21
-29
-53
+10
-11
+8
-10
-11
+12
-17
-25
-35
+1
-2
+14
+36
+70
+14
+50
+3
+6
+23
-20
-6
-66
-39
-14
+191
-17
+24
+155
-6
+61
+59623
-4
+7
+19
+5
-1
-8
-8
-15
+16
+8
-7
+19
-3
+14
+14
+6
+17
-15
+3
+11
+12
-5
+19
+2
+1
+16
+13
+6
+1
+18
+16
-4
+11
-1
+19
-7
-19
-10
+19
+7
+4
-17
-4
-4
+9
-8
-17
-9
-14
+16
+4
-19
+11
+1
+2
-19
+7
-8
-7
-18
-6
+4
-5
-15
+2
+7
-18
-8
+14
-20
+1
+17
-2
+3
+17
-1
+7
+10
+10
-1
+19
+3
-16
-10
+1
+10
+6
+20
-17
+19
+14
-18
-11
+22
-3
-16
+15
+12
+14
-12
-1
+22
+10
-7
-8
+2
-1
-6
-7
+19
+7
+12
-14
+18
+7
-17
-9
+10
+14
-1
-17
+8
+4
+2
+16
+3
+16
+4
-2
+18
-19
-15
-1
-10
-5
+8
-4
-13
+18
+15
-5
+10
-6
+13
-11
+12
+2
+4
-2
-13
-7
-11
-13
-5
+4
+10
+12
-15
-16
-10
-9
-27
-4
-9
-21
-13
-4
-1
-10
-10
+14
+12
-29
-16
-12
-1
-12
+18
-20
-11
-5
+12
-2
-6
+18
-2
-1
+6
-18
-12
+20
-4
+6
-14
+6
-17
-6
+8
-14
-18
-4
-8
+2
+7
+20
-10
-15
+18
-12
+13
-18
+9
+11
-19
+9
+3
+10
-17
-16
-4
-11
-5
-9
-12
+4
-13
+14
+13
-17
-21
+7
+19
+10
-6
+9
+13
-5
-13
+19
+7
-4
-6
+17
-16
-8
-14
-11
-3
-16
+10
-1
-2
-19
+3
+13
-15
+8
+6
-19
+2
-12
-6
-19
+17
-8
+11
+2
-18
-6
-10
+7
+4
+12
-3
+8
+7
+18
-8
+10
-15
-2
-12
+20
-13
-16
-11
+9
-1
+11
+19
-5
+20
-2
+17
+17
+17
+18
+17
+25
-31
+20
-7
+21
+15
+15
-13
+18
+14
+28
-10
+16
-17
-16
-2
-7
-8
-23
-16
+11
-20
+11
-3
-31
-4
-20
+18
-8
+3
+6
-15
-35
+19
+2
-4
+9
-12
-15
-9
-3
+15
+38
-6
+11
-36
-11
-14
-1
-5
+2
+23
-12
-33
-18
+1
+5
+3
+16
+7
-28
+8
-22
-7
-18
-17
+16
+11
+16
+4
-15
+7
-10
-7
-9
-4
+9
+6
-21
-11
-20
-19
-5
+3
+27
+9
+23
+16
-19
-2
+20
+13
-2
-16
+11
+2
-10
-11
-21
-6
-8
-10
-20
+35
+46
+2
-19
-13
+23
-2
+56
+7
+53
+5
-64
-62
+103
+162
+4
+10
-27
-15
-14
+22
+9
+1
+38
-9
+8
+9
+34
+1
-11
+4
-19
-29
+16
+52
-16
+27
+188
-83
+191
+59071
+19
-10
-6
-16
-18
+14
-10
+18
+4
-10
+3
+16
+6
-18
-3
+1
+4
-20
+4
-16
-7
-6
+19
+12
-3
-7
-13
-7
-9
+19
-20
+12
+12
+18
-15
-8
-11
-9
-7
+13
+16
+11
+4
-13
-10
+1
-11
-14
+13
+4
+10
-5
+7
-24
-7
+11
-22
-20
+15
-10
+13
-7
-25
-2
-4
+45
-1
+16
+11
+24
+4
+4
+22
-17
+13
-16
+5
+16
+16
+15
-119289")