(defun c:EARTH ( / *error* GeoMarker PT->LL errMsg earthPath ptMid ptLL strKML fName f)
;; Opens Google Earth. If dwg is Geo-Located, attempts to open at user-specified location.
  ;; Helper Function(s)
  ;; Error handler
  (defun *error* (msg / )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    );if
    (princ)
  );error defun
  (defun GeoMarker ( / ) ;; Creates arbitrary GeoMarker, returns ename
    (entmakex '((0 . "POSITIONMARKER") (100 . "AcDbEntity") (100 . "AcDbGeoPositionMarker") (90 . 0) (10 0.0 0.0 0.0) (40 . 1.0)
               (1 . "") (40 . 0.5) (290 . 0) (280 . 0) (290 . 1) (101 . "Embedded Object") (100 . "AcDbEntity") (100 . "AcDbMText")
               (10 0.1 0.1 0.0) (40 . 1.0) (1 . "") (210 0.0 0.0 1.0) (11 1.0 0.0 0.0) (42 . 9761.9) (43 . 6666.67)))
  );GeoMarker defun
  (defun PT->LL (pt / e luprec return) ;; Returns Longitude & Latitude of point provided
    (if (setq e (GeoMarker))
      (progn
        (setq e (vlax-ename->vla-object e))
        (vlax-put-property
          e
          'Position
          (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray vlax-vbdouble '(0 . 2)) pt))
        );vlax
        (setq luprec (getvar 'LUPREC))
        (setvar 'LUPREC 8)
        (setq return
          (mapcar
            'atof
            (list (vla-get-Longitude e) (vla-get-Latitude e) "0.")
          );mapcar
        );setq
        (setvar 'LUPREC luprec)
        (vla-delete e)
        return
      );progn
    );if
  );PT->LL defun
  ;; Initial checks
  (cond
    ((not (setq earthPath (vl-registry-read "HKEY_LOCAL_MACHINE\\SOFTWARE\\Google\\Google Earth Pro" "InstallLocation")))
      (setq errMsg "\nCould not locate Google Earth application.")
    );cond 1
    ((not (eq "Model" (getvar 'CTAB))) (setq errMsg "\nMust be in Model space to select location."))
    ((eq "" (getvar 'CGEOCS)) (setq errMsg "\nDrawing is not Geo-Located."))
  );cond
  (if earthPath (setq earthPath (strcat earthPath "googleearth.exe")))
  ;; Open Earth now if path found and error found, or test for other error.
  (cond
    ((and earthPath errMsg) (startapp earthPath) (prompt errMsg) (exit))
    (errMsg (prompt errMsg) (alert errMsg) (exit))
  );cond
  (prompt "\nPreparing to open location in Google Earth...")
  ;; User-selected or arbitrary mid point of project data
  (setq ptMid
    (cond ((getpoint "\nSelect Point or (Enter) for default: "))
          ((getvar 'VIEWCTR))
    );cond
  );setq
  ;; Transform to Lat Long
  (setq ptLL (PT->LL ptMid))
  (print ptLL)
  ;string for KML file
  (if *earth_marker_count*
    (setq *earth_marker_count* (1+ *earth_marker_count*))
    (setq *earth_marker_count* 1)
  );if
  (setq strKML
    (strcat
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns:kml=\"http://www.opengis.net/kml/2.2\" xmlns:atom=\"http://www.w3.org/2005/Atom\">"
      "  <Placemark>"
      "    <name>" (getvar 'DWGNAME) " (" (itoa *earth_marker_count*) ")</name>"
      "    <Point>"
      "      <coordinates>" (rtos (car ptLL) 2 7) "," (rtos (cadr ptLL) 2 7) ",0</coordinates>"
      "    </Point>"
      "  </Placemark>"
      "</kml>"
    );strcat
  );setq
  ;; Write string to KML file
  (setq fName (vl-filename-mktemp "C3D" nil ".kml"))
  (write-line strKML (setq f (open fName "w")))
  (close f)
  ;; Open KML in Google Earth
  (startapp earthPath fName)
  (prompt "\nEARTH Complete.")
  (princ)
);defun