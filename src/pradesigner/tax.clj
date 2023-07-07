(ns pradesigner.tax
  (:gen-class)
  (:require [tablecloth.api :as tc]))

(def colname-map {"column-0" :date
                  "column-1" :detail
                  "column-2" :debit
                  "column-3" :credit
                  "column-4" :card})



(def visa (-> (tc/dataset "resources/2020/cibc-visa.csv" {:header-row? false})
              (tc/rename-columns colname-map)
              (tc/replace-missing [:debit :credit] :value 0)))

(def sav (-> (tc/dataset "resources/2020/cibc-sav.csv" {:header-row? false})
             (tc/rename-columns (into {} (take 4 colname-map)))
             (tc/replace-missing [:debit :credit] :value 0)))

(def chq (-> (tc/dataset "resources/2020/cibc-chq.csv" {:header-row? false})
             (tc/rename-columns (into {} (take 4 colname-map)))
             (tc/replace-missing [:debit :credit] :value 0)))


#_(def mc (-> (tc/dataset "resources/2020/mc2020.csv" {:header-row? false})
            (tc/drop-rows 0)
            (tc/drop-columns ["column-2" "column-3"])
            (tc/rename-columns (conj (into {} (take 2 colname-map))
                                     {"column-4" :debit}))
            (tc/convert-types :debit :float64)
            (as-> d
              (tc/add-column d :credit (map (fn [x] (if (pos? x) x 0)) (d :debit))))
            (tc/update-columns :debit (partial map (fn [x] (if (pos? x) 0 x))))
            (tc/update-columns :debit (partial map #(* -1 %)))
            (tc/convert-types [:debit :credit] :float64)))

(def mc (-> (tc/dataset "resources/2020/mc2020.csv" {:header-row? false})
            (tc/drop-rows 0)
            (tc/drop-columns ["column-2" "column-3"])
            (tc/rename-columns (conj (into {} (take 2 colname-map))
                                     {"column-4" :debit}))
            (tc/convert-types :debit :float64)
            (tc/separate-column :debit [:debit :credit]
                                (fn [x] (if (neg? x)
                                          [(- x) 0.0]
                                          [0.0 x])))))

(tc/head mc)
;; => resources/2020/mc2020.csv [5 4]:

;; |    :date |                                  :detail |   :debit |  :credit |
;; |----------|------------------------------------------|----------|----------|
;; | 05/22/20 |         Interest Charge on Cash Advances |     0.00 |     0.00 |
;; | 05/22/20 |             Interest Charge on Purchases |     0.00 |     0.00 |
;; | 05/22/20 |                       REWARDS REDEMPTION |     0.00 |    48.16 |
;; | 05/22/20 | PAYPAL *HOMEDEPOTCA      4029357733   ON |    46.76 |     0.00 |
;; | 05/22/20 |                                PRAD BASU |     0.00 |     0.00 |


(tc/head visa)
;; => resources/2020/cibc-visa.csv [5 5]:

;; |              :date |                            :detail |   :debit |  :credit |            :card |
;; |--------------------|------------------------------------|----------|----------|------------------|
;; |         2020-12-31 |  PAYMENT THANK YOU/PAIEMEN T MERCI |          |  2194.52 | 4500********0045 |
;; |         2020-12-31 |         PC EXPRESS 1563 DUNCAN, BC |    16.39 |          | 4500********0045 |
;; |         2020-12-30 |       CANADIAN PROTEIN WINDSOR, ON |   119.68 |          | 4500********0045 |
;; |         2020-12-30 |         PC EXPRESS 1563 DUNCAN, BC |   196.57 |          | 4500********0045 |
;; |         2020-12-29 | PAYPAL *SHOPPERPLUS 4029357733, QC |    98.01 |          | 4500********0045 |

(tc/head sav)
;; => resources/2020/cibc-sav.csv [5 4]:

;; |              :date |                                                    :detail |   :debit |  :credit |
;; |--------------------|------------------------------------------------------------|----------|----------|
;; |         2020-12-31 |                                Branch Transaction INTEREST |          |    30.56 |
;; |         2020-12-31 | Internet Banking E-TRANSFER 010235441538 Robert Kowalewski |          |   100.00 |
;; |         2020-12-31 |         Electronic Funds Transfer PAY PAYROLL DEPOSIT UVIC |          |   359.94 |
;; |         2020-12-29 |            Internet Banking INTERNET TRANSFER 000000283687 |  2194.52 |          |
;; |         2020-12-24 |        Internet Banking E-TRANSFER 010232055388 Kyron Basu |          |   741.76 |


#_(defn itm-flag
  "flags an item based on keywords in :detail"
  [ds]
  (tc/add-column ds :flag (cond
                            (re-find #"DTI" (ds :detail)) "K")))


(def cP "0045")
(def cR "7361")
(def cK "3399")

(def Ks (re-pattern "DTI|MEMORY EXPRESS|STAPLES|GOOGLE"))
(def Rs (re-pattern "ISLAND PHARMACY|CHATTERS|SPRINGERUS|BC ASSOCIATION|BOOKDEPOSIT|GOOGLE STORAGE|AK PRESS"))
(def Hs (re-pattern "QUALITY BOX|SHAW CABLE|CANADIAN TAX AC|GREENPARTYC"))

(defn itm-flag
  "flags an item based on keywords in :detail"
  [ds]
  (tc/map-columns ds
                  :flag
                  :detail
                  (fn [& rows]
                    (let [r (first rows)]
                      (cond
                        (re-find Rs r) "R"
                        (re-find Ks r) "K"
                        (re-find Hs r) "H"
                        :else nil)))))

(tc/select-rows (itm-flag mc) (comp #(= % "H") :flag))
(tc/select-rows (itm-flag mc) (comp #(= % "K") :flag))
(tc/select-rows (itm-flag visa) (comp #(= % "K") :flag))

(-> (itm-flag mc)
    (tc/group-by :flag :as-map)
    (tc/columns))

(reduce + ((tc/select-rows mc (comp #(re-find (re-pattern "QUALITY BOX") %) :detail)) :debit))

(defn tots
  "gets totals of :debit :credit for given :detail search string in a dataset"
  [ds ss]
  (let [findfn (comp #(re-find (re-pattern ss) %) :detail)
        debits (:debit (tc/select-rows ds findfn :debit))
        credits (:credit (tc/select-rows ds findfn :credit))
        dtot (reduce + debits)
        ctot (reduce + credits)
        tot (.doubleValue (- dtot ctot))]
    {ss (float tot)}))



(def house ["QUALITY BOX" "SHAW CABLE" "CANADIAN TAX AC" "GREENPARTYC" "BC HYDRO"])
(def kyron ["DTI" "MEMORY EXPRESS" "STAPLES" "GOOGLE"])
(def ranjana ["ISLAND PHARMACY" "CHATTERS" "SPRINGERUS" "BC ASSOCIATION" "BOOKDEPOSIT" "GOOGLE STORAGE" "AK PRESS"])

(map #(comp (tots mc %)) house)
;; => ({"QUALITY BOX" 745.5} {"SHAW CABLE" 222.55} {"CANADIAN TAX AC" 149.0} {"GREENPARTYC" 0.0} {"BC HYDRO" 0.0})
(map #(comp (tots visa %)) house)
;; => ({"QUALITY BOX" 786.25} {"SHAW CABLE" 532.35} {"CANADIAN TAX AC" 0.0} {"GREENPARTYC" 125.0} {"BC HYDRO" 0.0})
(map #(comp (tots sav %)) house)
;; => ({"QUALITY BOX" 0.0} {"SHAW CABLE" 0.0} {"CANADIAN TAX AC" 0.0} {"GREENPARTYC" 0.0} {"BC HYDRO" 0.0})
(map #(comp (tots chq %)) house)
;; => ({"QUALITY BOX" 0.0} {"SHAW CABLE" 0.0} {"CANADIAN TAX AC" 0.0} {"GREENPARTYC" 0.0} {"BC HYDRO" 1178.59})


(map #(comp (tots mc %)) kyron)
;; => ({"DTI" 2547.45} {"MEMORY EXPRESS" 0.0} {"STAPLES" 330.98} {"GOOGLE" 2.79})
(map #(comp (tots visa %)) kyron)
;; => ({"DTI" 118.7} {"MEMORY EXPRESS" 706.16} {"STAPLES" 78.38} {"GOOGLE" 262.02})

(map #(comp (tots mc %)) ranjana)
;; => ({"ISLAND PHARMACY" 35.85} {"CHATTERS" 0.0} {"SPRINGERUS" 46.15} {"BC ASSOCIATION" 75.0} {"BOOKDEPOSIT" 269.87} {"GOOGLE STORAGE" 2.79} {"AK PRESS" 0.0})
(map #(comp (tots visa %)) ranjana)
;; => ({"ISLAND PHARMACY" 71.7} {"CHATTERS" 57.17} {"SPRINGERUS" 0.0} {"BC ASSOCIATION" 0.0} {"BOOKDEPOSIT" 0.0} {"GOOGLE STORAGE" 0.0} {"AK PRESS" 15.84})


;; house
                                        ; heating
(+ 745.5 786.25 1178.59) ;; => 2710.34
                                        ; soho
(+ 222.55 532.35) ;; => 754.90
                                        ; gpc
;; 125

;; quality box
~1500
;; bchydro bills:
Nov 20, 2019 to Jan 17, 2020: 503.99 -> 250
Jan 18, 2020 to Mar 18, 2020: 763.38
Mar 19, 2020 to May 15, 2020: 597.33
May 19, 2020 to Sep 16, 2020: 433.51
Sep 17, 2020 to Nov 17, 2020: 158.41
Nov 18, 2020 to Jan 18, 2021: 439.35 -> 200
total ~2400

;; kyron
                                        ; computer equip
(+ 2547.45 118.7 706.16) ;; => 3372.31
                                        ; google
(+ 2.79  262.02) ;; => 264.81 (books may be? or duplications?)


;; ranjana
                                        ; pharmacy
(+ 35.85 71.7);; => 107.55 and last year 143.40
                                        ; association
;;75
                                        ; books
(+ 46.15 269.87 15.84);; => 331.86


( -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))

