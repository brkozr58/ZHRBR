class ZHRBR_CL001 definition
  public
  final
  create public .

public section.

  class-methods BRUT_NET_ORAN
    importing
      value(IT) type HRPAYTR_IT optional
      value(RT) type HRPAY99_RT optional
      value(BUKRS) type BUKRS
      value(BEGDA) type BEGDA default SY-DATUM
      value(ENDDA) type ENDDA default SY-DATUM
    exporting
      value(ORAN) type BETRG .
  class-methods ODENEN_BORC
    importing
      value(ICRID) type ZHRBR_DE030
      value(BEGDA) type BEGDA default SY-DATUM
      value(ENDDA) type ENDDA default SY-DATUM
    exporting
      value(ICODM) type BETRG .
  class-methods ORAN_MAX_KESINTI
    importing
      value(IT) type HRPAYTR_IT
      value(RT) type HRPAY99_RT
      value(ORAN) type BETRG
      value(BUKRS) type BUKRS
      value(BEGDA) type BEGDA default SY-DATUM
      value(ENDDA) type ENDDA default SY-DATUM
      value(S9950) type PA9950
    exporting
      !ORNKS type BETRG .
  class-methods YASAL_NET
    importing
      value(IT) type HRPAYTR_IT optional
      value(RT) type HRPAY99_RT optional
      value(BUKRS) type BUKRS
    exporting
      !V550 type MAXBT
      !VABR type ABART
      !VAPZ type APZNR .
protected section.
private section.
ENDCLASS.



CLASS ZHRBR_CL001 IMPLEMENTATION.


METHOD BRUT_NET_ORAN.
  DATA : t005     TYPE TABLE OF zhrbr_t005.
  DATA : s005     TYPE zhrbr_t005.
  DATA : st       TYPE pc207.
  DATA : gv_betrg TYPE betrg.

  DATA : gv_pay   TYPE pc207-betrg.          "Net için Pay
  DATA : gv_pyd   TYPE pc207-betrg.          "Net için Payda

  CLEAR: t005[] , s005 , gv_pay , gv_pyd.

  SELECT * FROM zhrbr_t005 INTO TABLE t005
                              WHERE bukrs EQ bukrs
                                AND begda LE endda
                                AND endda GE begda.

  LOOP AT t005 INTO s005.
    CLEAR gv_betrg.
    LOOP AT rt INTO st WHERE lgart EQ s005-lgart.
      gv_betrg = st-betrg + gv_betrg.
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT it INTO st WHERE lgart EQ s005-lgart.
        gv_betrg = st-betrg + gv_betrg.
      ENDLOOP.
    ENDIF.
    CASE s005-paypd.
      WHEN 'PY'."Pay
        IF s005-islem EQ '-'.
          gv_pay = gv_pay - gv_betrg.
        ELSE .
          gv_pay = gv_pay + gv_betrg.
        ENDIF.
      WHEN 'PD'."Payda
        IF s005-islem EQ '-'.
          gv_pyd = gv_pyd - gv_betrg.
        ELSE .
          gv_pyd = gv_pyd + gv_betrg.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  IF gv_pyd GT 0.
    oran = ( gv_pay * 100 ) / gv_pyd.
  ENDIF.
ENDMETHOD.


method ODENEN_BORC.
  DATA : t9951 TYPE TABLE OF pa9951.
  DATA : s9951 TYPE pa9951.

  SELECT * FROM pa9951 INTO TABLE t9951
                            WHERE icrid EQ icrid
*{   REPLACE        MIDK902391                                        1
*\                              AND begda LT begda .
                              AND begda Lt begda . "16.09.2024
*}   REPLACE

  LOOP AT t9951 INTO s9951.
    icodm = icodm + s9951-odmtr.
  ENDLOOP.

endmethod.


METHOD oran_max_kesinti.

  DATA : t004    TYPE TABLE OF zhrbr_t004.
  DATA : s004    TYPE zhrbr_t004.
  DATA : st      TYPE pc207.
  DATA : lv_orng TYPE zhrbr_de006 ."Oran Grubu
  DATA : lv_orny TYPE zhrbr_de015 ."Oran Yüzde
  SELECT * FROM zhrbr_t004 INTO TABLE t004
                                WHERE begda LE endda
                                  AND endda GE begda
                                  AND bukrs EQ bukrs.

  DO 9 TIMES VARYING lv_orng FROM s9950-orng1 NEXT s9950-orng2
             VARYING lv_orny FROM s9950-orny1 NEXT s9950-orny2.
    IF lv_orng IS NOT INITIAL AND lv_orny IS NOT INITIAL.
      LOOP AT t004 INTO s004 WHERE orngr EQ lv_orng."Net
        CLEAR : st.
        LOOP AT rt INTO st WHERE lgart EQ s004-lgart_nt.
          IF s004-islem EQ '-'.
            IF st-betrg GT 0.
              st-betrg = st-betrg * -1.
            ENDIF.
          ENDIF.
          ornks = ornks + ( ( st-betrg * lv_orny ) / 100 ).
        ENDLOOP.
        IF sy-subrc NE 0."Brüt
          LOOP AT rt INTO st WHERE lgart EQ s004-lgart.
            IF s004-islem EQ '-'.
              IF st-betrg GT 0.
                st-betrg = st-betrg * -1.
              ENDIF.
            ENDIF.
            ornks = ornks + ( ( ( ( st-betrg * lv_orny ) / 100 ) * oran ) / 100 ).
          ENDLOOP.
          IF sy-subrc NE 0.
            LOOP AT it INTO st WHERE lgart EQ s004-lgart_nt."Net
              IF s004-islem EQ '-'.
                IF st-betrg GT 0.
                  st-betrg = st-betrg * -1.
                ENDIF.
              ENDIF.
              ornks = ornks + ( ( st-betrg * lv_orny ) / 100 ).
            ENDLOOP.
            IF sy-subrc NE 0.
              LOOP AT it INTO st WHERE lgart EQ s004-lgart."Brüt
                IF s004-islem EQ '-'.
                  IF st-betrg GT 0.
                    st-betrg = st-betrg * -1.
                  ENDIF.
                ENDIF.
                ornks = ornks + ( ( ( ( st-betrg * lv_orny ) / 100 ) * oran ) / 100 ).
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
*{   INSERT         ISDK935744                                        1
*    IF ornks LE 0.
*      CLEAR ornks.
*    ENDIF.
*}   INSERT
    CLEAR : lv_orng , lv_orny.
  ENDDO.

ENDMETHOD.


  METHOD YASAL_NET.
    DATA : t007     TYPE TABLE OF zhrbr_t007.
    DATA : s007     TYPE zhrbr_t007.
    DATA : st       TYPE pc207.
    DATA : gv_betrg TYPE betrg.

    CLEAR: t007[] , s007 .
    SELECT * FROM zhrbr_t007 INTO TABLE t007
                                  WHERE bukrs EQ bukrs.

    CLEAR gv_betrg.

    LOOP AT t007 INTO s007.

    LOOP AT rt INTO st WHERE lgart EQ s007-lgart.
      IF s007-islem EQ '-'.
        gv_betrg = gv_betrg - st-betrg.
      ELSE .
        gv_betrg = gv_betrg + st-betrg.
      ENDIF.
      IF vabr IS INITIAL.
       vabr = st-abart.
       vapz = st-apznr.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0.
      LOOP AT it INTO st WHERE lgart EQ s007-lgart.
        IF s007-islem EQ '-'.
          gv_betrg = gv_betrg - st-betrg.
        ELSE .
          gv_betrg = gv_betrg + st-betrg.
        ENDIF.
        IF vabr IS INITIAL.
         vabr = st-abart.
         vapz = st-apznr.
        ENDIF.
      ENDLOOP.
    ENDIF.
    ENDLOOP.

    v550 = gv_betrg.
  ENDMETHOD.
ENDCLASS.
