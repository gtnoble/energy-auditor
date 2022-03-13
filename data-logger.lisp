(in-package :data-logger)

(ql:quickload "mito")
(mito:connect-toplevel :sqlite3 :database-name "data-log.db")

(mito:deftable reading () 
               ((source :col-type :text)
                (channel :col-type :text)
                (value :col-type :real)
                (notes :col-type :text)
                ))

(mito:ensure-table-exists 'reading)

(defun record-reading (instrument channel value notes)
  (mito:create-dao 'reading 
                   :instrument instrument 
                   :channel channel 
                   :value value 
                   :notes notes
                   ))

