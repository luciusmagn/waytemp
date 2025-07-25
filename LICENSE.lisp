;; Configurable Open Lisp License (COLL)
;;
;; TEMPLATE NOTICE: This license framework may be freely used as a template
;; by changing the copyright holder, year, and selection of permissions,
;; conditions and limitations in the function below. The wording of the legal
;; terms themselves must remain unmodified to preserve legal clarity.
;; Use of this license template is at your own risk.

(asdf:load-system :serapeum)
(use-package :serapeum)

(defun license ()
  (~> '((copyright   . "2025 Lukáš Hozda")
        (permissions . (use copy modify distribute sell))
        (conditions  . (retain-notice))
        (limitations . (no-warranty no-liability)))
      (dict)
      (progn
        (dict-insert 'variant
                     (cond
                       ((null (@ _ 'copyright))                                    "COLL-PublicDomain")
                       ((subset '(same-license disclose-source) (@ _ 'conditions)) "COLL-Copyleft")
                       ((member 'same-license (@ _ 'conditions))                   "COLL-ShareAlike")
                       ((not (member 'sell (@ _ 'permissions)))                    "COLL-NonCommercial")
                       ((member 'retain-notice (@ _ 'conditions))                  "COLL-Attribution")
                       (t                                                          "COLL-Permissive"))
                     _))))

;; LEGAL TERMS AND CONDITIONS
;;
;; Active terms are those returned by (license) in the lists for
;; 'permissions', 'conditions', and 'limitations'. Terms not present
;; in those lists do not apply, regardless of being described below.
;;
;; COPYRIGHT:
;; Copyright 2025 Lukáš Hozda. All rights reserved except as granted above.
;;
;; PERMISSIONS - The following rights are granted:
;; [use]          The right to use the Software for any purpose, commercial or non-commercial
;; [copy]         The right to make copies of the Software in any medium
;; [modify]       The right to create derivative works based on the Software
;; [distribute]   The right to distribute copies of the Software to third parties
;; [sell]         The right to sell copies of the Software or charge for distribution
;; [patent-grant] Any patents held by copyright holder covering the Software are licensed
;;
;; CONDITIONS - Exercise of permissions is subject to:
;; [retain-notice]    This copyright notice and license must be included in all copies
;; [disclose-source]  Source code must be made available when distributing the Software
;; [same-license]     Derivative works must be licensed under identical terms
;; [document-changes] Modifications must be clearly documented with date and author
;; [network-use]      Use of the Software over a network constitutes distribution
;;
;; LIMITATIONS - The following disclaimers apply:
;; [no-warranty]  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;                  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO WARRANTIES OF
;;                  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT
;; [no-liability] IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM,
;;                  DAMAGES OR OTHER LIABILITY ARISING FROM USE OF THE SOFTWARE
;; [no-trademark] No trademark rights are granted under this license
