*&---------------------------------------------------------------------*
*&  Include           ZHRBR_BORCLAR
*&---------------------------------------------------------------------*

DATA : gv_v0 TYPE numc2.

*&---------------------------------------------------------------------*
*&      Form  FUZBORC
*&---------------------------------------------------------------------*
FORM fuzhrbr .
  CASE as-parm1.
    WHEN '01'.
      PERFORM borc_hesaplama.
    WHEN '02'.
      PERFORM borc_bt_insert.
  ENDCASE.
ENDFORM.                    " FUZBorc
*&---------------------------------------------------------------------*
*&      Form  Borc_HESAPLAMA
*&---------------------------------------------------------------------*
FORM borc_hesaplama .
*--------------------------------------------------------------------*
*--- Create By OkanK.             Mail : okan.keles@detaysoft.com    *
*--------------------------------------------------------------------*
***************** MANUEL DEĞİŞİKLİK YAPMAYINIZ!!!!********************
*--------------------------------------------------------------------*
* Data Def..
*--------------------------------------------------------------------*
  DATA : BEGIN OF s9950 .
           INCLUDE TYPE pa9950.
  DATA :   icksr TYPE zhrbr_de001,
           kaluc TYPE zhrbr_de002,
           ksluc TYPE zhrbr_de031,
         END OF s9950.

  DATA : ls9950  TYPE pa9950.
  DATA : t9950   LIKE TABLE OF s9950 .      "İcralar
  DATA : t9951   TYPE TABLE OF pa9951.      "Kesintiler
  DATA : s002    TYPE zhrbr_t002.           "Sonrakine Geçiş
  DATA : t005    TYPE TABLE OF zhrbr_t005.  "Brüt->Net Hesap

  DATA : gv_ynet  TYPE pc207-betrg.          "Toplam /550
  DATA : gv_abr  TYPE abart.                "Abart
  DATA : gv_apz  TYPE apznr.                "Apznr
  DATA : gv_orn  TYPE betrg.                "Net için Oran

  DATA : gv_oks  TYPE betrg.                "Oranlı İcra Max Kesinti
  DATA : gv_ks1  TYPE betrg.                "Oranlı İcra Max Kesinti
  DATA : gv_top  TYPE betrg.                "Toplam İcra
  DATA : gv_ctp  TYPE betrg.                "Kontrol Toplam İcra
  DATA : gv_odm  TYPE betrg.                "Ödenen İcra
  DATA : gv_kln  TYPE betrg.                "Kalan  İcra
  DATA : gv_kln2  TYPE betrg.                "Kalan  İcra
  DATA : gv_odc  TYPE betrg.                "Ödenecek  Tutar
  DATA : gv_ksk  TYPE betrg.                "Kesilemeyen Kesinti

  DATA : gv_sbt  TYPE subty.                "Max kesinti için kontrol
  DATA : gv_nxt  TYPE char1.                "Sonrakine Geçiş Kontrolü

  DATA : gt_man  TYPE TABLE OF pa9951.      "Manuel Kesintiler
  DATA : gs_man  TYPE pa9951.               "Manuel Kesintiler
*--------------------------------------------------------------------*
  SELECT * FROM pa9950 AS p9 LEFT OUTER JOIN zhrbr_t001 AS t1
                       ON p9~subty EQ t1~subty
                     INTO CORRESPONDING FIELDS OF TABLE t9950
                    WHERE p9~pernr EQ pernr-pernr
                      AND p9~begda LE aper-endda
                      AND p9~endda GE aper-begda
                      AND p9~sprps EQ space
                      AND p9~compl EQ space.

  CHECK sy-subrc EQ 0.
  CLEAR gv_ynet.


  SELECT * FROM pa9951 INTO TABLE gt_man
                            WHERE pernr EQ pernr-pernr
                              AND begda LE aper-endda
                              AND endda GE aper-begda
                              AND manue EQ 'X'.

  "Yasal Net hesaplama
  CALL METHOD zhrbr_cl001=>yasal_net
    EXPORTING
      it    = it[]
      rt    = rt[]
      bukrs = pernr-bukrs
    IMPORTING
      v550  = gv_ynet
      vabr  = gv_abr
      vapz  = gv_apz.

  CHECK gv_ynet GT 0.
  "Brüt->Net Oran hesaplama
  CALL METHOD zhrbr_cl001=>brut_net_oran
    EXPORTING
      it    = it[]
      rt    = rt[]
      bukrs = pernr-bukrs
      begda = aper-begda
      endda = aper-endda
    IMPORTING
      oran  = gv_orn.

*-- İcra Hesaplamaları
*--------------------------------------------------------------------*

  "Kesinti sırası sıralaması
  SORT t9950 ASCENDING BY icksr kessr.
  CLEAR:gv_sbt , gv_nxt , gv_ksk.

  LOOP AT t9950 INTO s9950.
    SELECT SINGLE * FROM zhrbr_t002 INTO s002
                                   WHERE bukrs EQ pernr-bukrs
                                     AND subty EQ s9950-subty.
    "Sonraki döneme geçmesin
    IF gv_sbt EQ s9950-subty AND s9950-hspsk EQ '04'
             AND ( s002-dnmkn EQ '2' OR gv_oks IS INITIAL ).
      CHECK 1 = 2.
    ENDIF.
    "Kesilen
    CALL METHOD zhrbr_cl001=>odenen_borc
      EXPORTING
        icrid = s9950-icrid
        begda = aper-begda
        endda = aper-endda
      IMPORTING
        icodm = gv_odm.

    CLEAR gv_ctp.
    gv_ctp = ( s9950-icrtr + s9950-faizt )."Toplam İcra
    IF s9950-hspsk NE '01'."Sabit Kesinti Değilse
      CHECK gv_odm LT gv_ctp."Kesilen Borc daha azsa
    ENDIF.
    IF s9950-hspsk EQ '01'."Sabit Kesinti
      gv_top = s9950-tkstt.
      gv_kln = s9950-tkstt.
      IF s9950-tkstt LE gv_ynet.
        gv_odc = s9950-tkstt.
      ELSE.
        gv_ksk = gv_odc - gv_ynet."Kesilemeyen kesinti
        gv_odc = gv_ynet.
      ENDIF.
    ELSEIF s9950-hspsk EQ '04'.   " Oran ile kesinti
      IF gv_sbt NE s9950-subty." Alt tip bazında 1 kere hesapla
        CLEAR ls9950.
        MOVE-CORRESPONDING s9950 TO ls9950.
        CALL METHOD zhrbr_cl001=>oran_max_kesinti
          EXPORTING
            it    = it[]
            rt    = rt[]
            oran  = gv_orn "Brüt->Net oran
            bukrs = pernr-bukrs
            begda = aper-begda
            endda = aper-endda
            s9950 = ls9950
          IMPORTING
            ornks = gv_oks. "Oran ile max kesinti
      ENDIF.
      gv_top = ( s9950-icrtr + s9950-faizt )."Toplam İcra
      gv_kln = gv_top - gv_odm .             "Kalan  İcra
      CLEAR gv_ks1.
      gv_ks1 = gv_oks.
      IF gv_ks1 GT gv_ynet.
        gv_ks1 = gv_ynet.
      ENDIF.
      CLEAR gs_man.
      READ TABLE gt_man INTO gs_man WITH KEY pernr = pernr-pernr
                                             icrid = s9950-icrid.
      IF gv_ks1 GE gv_kln.
        gv_odc = gv_kln.
        IF gs_man-manue EQ 'X'.
          gv_odc = gs_man-odmtr."Manuel Ödemeyse değiştirme
        ENDIF.
        gv_oks = gv_oks - gv_odc.
      ELSE.
        gv_odc = gv_ks1.
        IF gs_man-manue EQ 'X'.
*          gv_odc = gs_man-odmtr."Manuel Ödemeyse değiştirme
          gv_odc = gv_odc + gs_man-odmtr."Manuel Ödemeyse değiştirme "01.10.2024
        ENDIF.
        gv_oks = gv_oks - gv_odc.
*        gv_odc = trunc( gv_odc ).
**************        << repair add bozer 191120
        DATA : lv_odc TYPE i   .
        lv_odc  = gv_odc.
        gv_odc = lv_odc.
**************
      ENDIF.
    ELSE.   " Diğer kesintiler
      gv_top = ( s9950-icrtr + s9950-faizt )."Toplam İcra
      gv_kln = gv_top -  gv_odm .             "Kalan  İcra "..

      IF gv_kln LT s9950-tkstt.
        s9950-tkstt = gv_kln."Kalan taksitden daha az kaldıysa kalanı al
      ENDIF.

      IF s9950-tkstt LE gv_ynet.
        gv_odc = s9950-tkstt.
      ELSE.
        gv_odc = gv_ynet.
      ENDIF.
    ENDIF.

*    "--->add code  ~ begin
    IF s9950-hspsk NE '04'."Hesaplama Şekli oran olmayanlar içinde
      "Manuel kesintiyi kesmesi için yapıldı.
      LOOP AT gt_man INTO gs_man WHERE icrid EQ s9950-icrid
                                   AND begda GE aper-begda
                                   AND endda LE aper-endda.
      ENDLOOP.
      IF sy-subrc EQ 0.
        CLEAR gv_odc.
        LOOP AT gt_man INTO gs_man WHERE icrid EQ s9950-icrid
                                     AND begda GE aper-begda
                                     AND endda LE aper-endda.
          gv_odc = gv_odc + gs_man-odmtr.
          "--->add code   ~ begin
          "Manuel Kesinti için batch kaydı oluşturmaması için yapıldı.
*          gv_man = abap_true.
          "--<<add code  ~ end
        ENDLOOP.
      ENDIF.

    ENDIF.
**********************************************************************
    PERFORM add_it1 USING s9950-icrid   s9950-subty
                         s9950-kaluc   s9950-ksluc
                         gv_top gv_kln gv_odc  gv_abr  gv_ksk gv_apz.
    gv_ynet = gv_ynet - gv_odc.
    gv_sbt  = s9950-subty.


  ENDLOOP.

*--------------------------------------------------------------------*

ENDFORM.                    " Borc_HESAPLAMA
*&---------------------------------------------------------------------*
*&      Form  ADD_IT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_icrid  İcra ID
*      -->p_subty  İcra Ücret Türü
*      -->p_kaluc  Kalan Ücret için Tür
*      -->p_ksluc  Kesilemeyen Kesinti
*      -->p_top    Toplam Borç
*      -->p_kln    Kalan Borç
*      -->p_odc    Kesilecek Borç
*      -->p_abr    Abart
*      -->p_ksk    Kesilemeyen Kesinti
*----------------------------------------------------------------------*
FORM add_it1  USING    p_icrid p_subty p_kaluc p_ksluc
                      p_top   p_kln   p_odc   p_abr p_ksk p_apz.
  DATA : ls_it TYPE pc207.
  CLEAR ls_it .

  IF p_odc GT 0 .
    gv_v0 = gv_v0 + 1.
    ls_it-v0typ = v0-v0typ = 'Z'  .
    ls_it-v0znr = v0-v0znr = gv_v0 .
    v0-vinfo    = p_icrid .
    APPEND v0 .

    ls_it-abart = p_abr .
    ls_it-apznr = p_apz .
    ls_it-lgart = p_subty .
    ls_it-betrg = p_odc * -1 .
    APPEND ls_it TO it.

    IF p_ksluc IS NOT INITIAL.
      ls_it-abart = p_abr .
      ls_it-apznr = p_apz .
      ls_it-lgart = p_ksluc .
      ls_it-betrg = p_ksk .
      APPEND ls_it TO it.
    ENDIF.

    IF p_kaluc IS NOT INITIAL.

      ls_it-abart = p_abr .
      ls_it-apznr = p_apz .
      ls_it-lgart = p_kaluc .
      ls_it-betrg = ( p_kln - p_odc ) * -1 .
      APPEND ls_it TO it.

    ENDIF.
  ENDIF .

ENDFORM.                    " ADD_IT
*&---------------------------------------------------------------------*
*&      Form  Borc_BT_INSERT
*&---------------------------------------------------------------------*
*--------------------------------------------------------------------*
*--- Create By OkanK.             Mail : okan.keles@detaysoft.com    *
*--------------------------------------------------------------------*
***************** MANUEL DEĞİŞİKLİK YAPMAYINIZ!!!!********************
*--------------------------------------------------------------------*
* Data Def..
*--------------------------------------------------------------------*
FORM borc_bt_insert .
  DATA : lv_btr   TYPE char15.
  DATA : lv_bgd   TYPE char10.
  DATA : lv_end   TYPE char10.
  DATA : lv_tab   TYPE sy-tabix.
  DATA : t591  TYPE TABLE OF t591a.
  DATA : s591  TYPE t591a.
  DATA : lt_rt LIKE TABLE OF rt.

  DATA: lv_betrg LIKE p9951-odmtr.

  CHECK tst_on EQ space.

  CLEAR : t591.
  SELECT * FROM t591a INTO TABLE t591
                           WHERE infty EQ '9950'.

  LOOP AT v0 WHERE v0typ EQ 'Z'.
    LOOP AT rt WHERE v0typ EQ v0-v0typ
                 AND v0znr EQ v0-v0znr.
      lv_tab = sy-tabix.
      READ TABLE t591 INTO s591 WITH KEY subty = rt-lgart.
      CHECK sy-subrc EQ 0.

      IF h_bdc_adv IS INITIAL.
*        PERFORM bdc_init.
*        h_bdc_adv = 'X'.
      ENDIF.

      CLEAR : lv_betrg , lv_bgd , lv_end.

      IF rt-betrg LT 0.
*        lv_btr = rt-betrg * -1.
        lv_betrg = rt-betrg * -1.
      ELSE.
*        lv_btr = rt-betrg.
        lv_betrg = rt-betrg.
      ENDIF.
*      lv_btr = rt-betrg.
      WRITE aper-begda TO lv_bgd.
      WRITE aper-endda TO lv_end.
*      SHIFT lv_btr LEFT DELETING LEADING space.
*      TRANSLATE lv_btr USING '.,'.

      "yorum satırına alındı .
*      REFRESH bdcdata.
*      PERFORM bdc_data_prog USING 'SAPMP50A'    '1000'      'X'.
*      PERFORM bdc_data_scrn USING 'RP50G-PERNR' pernr-pernr ' '.
*      PERFORM bdc_data_scrn USING 'RP50G-CHOIC' '9951'      ' '.
*      PERFORM bdc_data_scrn USING 'BDC_OKCODE'  '=INS'      ' '.
*
*      PERFORM bdc_data_prog USING 'MP995100'    '2000'      'X'.
*      PERFORM bdc_data_scrn USING 'P9951-BEGDA' lv_bgd      ' '.
*      PERFORM bdc_data_scrn USING 'P9951-ENDDA' lv_end      ' '.
*      PERFORM bdc_data_scrn USING 'P9951-ICRID' v0-vinfo    ' '.
*      PERFORM bdc_data_scrn USING 'P9951-ODMTR' lv_btr      ' '.
*      PERFORM bdc_data_scrn USING 'BDC_OKCODE'  '=UPD'      ' '.

      "yorum satırına alındı .
      DATA : ls_9951        TYPE p9951,
             return         TYPE bapireturn1,
             key            TYPE  bapipakey,
             lv_beg         TYPE  begda,
             lv_enda        TYPE  begda,
             temp_btr       TYPE string,
             lv_beg_temp    TYPE  begda,
             lv_end_temp    TYPE  begda.

       CONCATENATE lv_bgd+6(4) lv_bgd+3(2) lv_bgd+0(2) INTO lv_beg .
       CONCATENATE lv_end+6(4) lv_bgd+3(2) lv_end+0(2) INTO lv_enda .

      DATA : s_9950 TYPE pa9950.
      SELECT SINGLE * FROM pa9950 INTO  s_9950
        WHERE pernr = pernr-pernr AND begda LE lv_enda
         AND icrid = v0-vinfo AND endda GE lv_beg . "17.09.2024
        DATA ls_9951_2 TYPE pa9951 .
              SELECT SINGLE * FROM pa9951 INTO  ls_9951_2 "01.10.2024
        WHERE pernr = pernr-pernr AND begda LE lv_enda
         AND icrid = v0-vinfo AND endda GE lv_beg
                AND manue EQ 'X'. ""01.10.2024
                IF sy-subrc EQ '0'."01.10.2024
                  lv_betrg = lv_betrg - ls_9951_2-odmtr. "01.10.2024
                ENDIF. "01.10.2024

      ls_9951-infty = '9951' .
      ls_9951-pernr = pernr-pernr .
      ls_9951-begda = lv_beg .
      ls_9951-endda = lv_enda . .
      ls_9951-icrid = v0-vinfo .
*       ls_9951-odmtr = s_9950-TKSTT .
      ls_9951-odmtr = lv_betrg .

*      temp_btr = lv_btr .
*      REPLACE ',' WITH '.' INTO temp_btr .
*      CONDENSE temp_btr .
*      ls_9951-odmtr = temp_btr .

      CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = pernr-pernr
        IMPORTING
          return = return.

      IF return-number IS INITIAL AND return-number EQ 0 .

        DATA : s_9951 TYPE pa9951.
        SELECT SINGLE * FROM pa9951 INTO s_9951
        WHERE pernr = pernr-pernr AND begda = lv_beg AND
              endda = lv_enda AND icrid = v0-vinfo .

        .
        IF sy-subrc EQ 0 .

          DATA : s9951 TYPE p9951 .
          s9951-infty = '9951' .
          s9951-pernr = s_9951-pernr .
          s9951-begda = lv_beg .
          s9951-endda = lv_enda . .
          s9951-icrid = s_9951-icrid .
          s9951-odmtr = s_9951-odmtr .
          s9951-subty = s_9951-subty .
          s9951-seqnr = s_9951-seqnr .
          s9951-aedtm = s_9951-aedtm .
          s9951-uname = s_9951-uname.


          CALL FUNCTION 'HR_INFOTYPE_OPERATION'
            EXPORTING
              infty         = '9951'
              number        = s9951-pernr
              subtype       = s9951-subty
              validityend   = s9951-endda
              validitybegin = s9951-begda
              recordnumber  = s9951-seqnr
              record        = s9951
              operation     = 'DEL'
              tclas         = 'A'
              dialog_mode   = '0'
            IMPORTING
              return        = return
              key           = key.


        ENDIF.

        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '9951'
            number        = pernr-pernr
            validityend   = lv_enda
            validitybegin = lv_beg
*           RECORDNUMBER  =
            record        = ls_9951
            operation     = 'INS'
            tclas         = 'A'
            dialog_mode   = '0'
          IMPORTING
            return        = return
            key           = key.


        CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
          EXPORTING
            number = pernr-pernr
* IMPORTING
*           RETURN =
          .
      ENDIF.
*      CALL FUNCTION 'BDC_INSERT'
*        EXPORTING
*          tcode     = 'PA30'
*        TABLES
*          dynprotab = bdcdata.

      APPEND rt TO lt_rt.
      DELETE rt INDEX lv_tab.CLEAR lv_tab.
    ENDLOOP.
  ENDLOOP.

  DATA : s_rt LIKE LINE OF lt_rt.
  LOOP AT lt_rt INTO s_rt.
    CLEAR : s_rt-v0typ , s_rt-v0znr.
    COLLECT s_rt INTO rt.
  ENDLOOP.

ENDFORM.                    " Borc_BT_INSERT
*&---------------------------------------------------------------------*
*&      Form  BDC_DATA_PROG
*&---------------------------------------------------------------------*
FORM bdc_data_prog  USING    prog dynp dynb.
  CLEAR bdcdata.
  bdcdata-program  = prog.
  bdcdata-dynpro   = dynp.
  bdcdata-dynbegin = dynb.
  APPEND bdcdata.
ENDFORM.                    " BDC_DATA_PROG
*&---------------------------------------------------------------------*
*&      Form  BDC_DATA_SCRN
*&---------------------------------------------------------------------*
FORM bdc_data_scrn  USING  fnam fval date.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  IF date EQ 'X'.
    WRITE fval TO bdcdata-fval.
  ELSE.
    bdcdata-fval = fval.
  ENDIF.
  APPEND bdcdata.
ENDFORM.                    " BDC_DATA_SCRN
*&---------------------------------------------------------------------*
*&      Form  BDC_INIT
*&---------------------------------------------------------------------*
FORM bdc_init .

  DATA: btci_name LIKE apqi-groupid.

*  WRITE 'Z_BORCLAR' TO btci_name.
*
*  CALL FUNCTION 'BDC_OPEN_GROUP'
*    EXPORTING
*      client = sy-mandt
*      group  = btci_name
*      user   = sy-uname.

ENDFORM.                    " BDC_INIT
*&---------------------------------------------------------------------*
*&      Form  BDC_CLOSE
*&---------------------------------------------------------------------*
FORM bdc_close .

  CALL FUNCTION 'BDC_CLOSE_GROUP'.

ENDFORM.                    " BDC_CLOSE
