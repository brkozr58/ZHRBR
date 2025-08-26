*&---------------------------------------------------------------------*
*& Include          zhrbr_P001_I005
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_DATE
*&---------------------------------------------------------------------*
FORM set_date  USING    p_date TYPE datum
               CHANGING s_date.
  CONCATENATE p_date+6(2) p_date+4(2) p_date(4) INTO s_date
                                                SEPARATED BY '.'.
ENDFORM.                    "set_date
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'GUI_100'.
*  SET TITLEBAR 'xxx'.

*  LOOP AT SCREEN.
*    IF screen-name+0(1) = 'P'.
*      screen-required = 'X'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDMODULE.                    "status_0100 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = sy-ucomm.

  CASE save_ok.
    WHEN 'BUTTON1'. " Sıraya Girecek Kesinti Cevap
      number = 110.
    WHEN 'BUTTON2'. " Kesinti Yapılacagına Dair Cevap
      number = 120.
    WHEN 'BUTTON3'. " Çıkış Bilgi
      number = 130.
    WHEN 'BUTTON4'. " Eski İşten Ayrılmış Personel
      number = 140.
    WHEN '&F03' OR 'F012'.
      LEAVE TO SCREEN 0.
    WHEN 'F015'.
      LEAVE PROGRAM .
    WHEN 'BUTTON5'.
      CLEAR gv_error.
      PERFORM control.

      IF gv_error IS INITIAL.
        gr_report->send_word( ).
      ENDIF.
  ENDCASE.
ENDMODULE.                    "user_command_0100 INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_110
*&---------------------------------------------------------------------*
FORM set_110 .
  DATA : l_context      TYPE string,
         l_icratnm      TYPE zhrbr_de029,
         l_icrano       TYPE zhrbr_de019,
         l_sirano       TYPE zhrbr_de001,
         l_sira(15),
         l_sumic(20),
         l_tabix(2)     TYPE c,
         lv_field(100)  TYPE c,
         lv_counter(40) TYPE c.
  DATA : lv_tabx TYPE char2 .
  DATA : lv_tabx1 TYPE char2 .

  TYPES: BEGIN OF l_9950,
           icrid TYPE zhrbr_de030,
           alici TYPE zhrbr_de024,
           kessr TYPE zhrbr_de001,
           dosno TYPE zhrbr_de019,
           evrth TYPE datum, "EVRAK DÜZENLEME TARİHİ
           icrad TYPE zhrbr_de020,
           compl TYPE zhrbr_de021, "AKTİF Mİ
           aborc TYPE betrg, "Ana Borç
           odmtr TYPE zhrbr_de022, "Kesilen Aylık Tutar
           bborc TYPE betrg,
*           logid         TYPE zhrpa_de014,
*           zhr_alici     TYPE zhrpa_de023,
*           zhr_ic_no     TYPE zhrpa_de015,
*           zhr_ic_dosno  TYPE zhrpa_de016,
*           zhr_ic_kestrh TYPE zhrpa_de052,
*           zhr_icrd      TYPE zhrpa_de022,
*           zhr_ic_act    TYPE zhrpa_de017,
*           zhr_sum_ic    TYPE zhrpa_de019,
*           zhr_new_icks  TYPE zhrpa_de020,
*           zhr_sum_ickl  TYPE zhrpa_de021,

         END OF l_9950.
  DATA: lt_9950 TYPE TABLE OF l_9950,
        ls_9950 TYPE          l_9950.

  SELECT * FROM pa9950 INTO TABLE gt_9950 WHERE pernr EQ p_pernr.
  " aktif olan icra
  READ TABLE gt_9950 INTO gs_9950 WITH KEY compl = ' '.
  IF sy-subrc EQ 0.
    lv_tabx = sy-tabix .
    lv_tabx1 = sy-tabix - 1.
    IF lv_tabx1 EQ '0'.
      CLEAR lv_tabx1.
    ENDIF.
    SELECT SINGLE icraa FROM zhrbr_t006 INTO l_icratnm
                            WHERE icrad EQ gs_9950-icrad.
    l_icrano = gs_9950-dosno.
    CONCATENATE l_icratnm l_icrano text-001
                INTO l_context SEPARATED BY space.
    CLEAR : l_icratnm, l_icrano.
  ENDIF.

  "sırasındaki icralar
  SORT gt_9950 BY kessr.
  LOOP AT gt_9950 INTO gs_9950 WHERE compl NE 'D'.
    SELECT SINGLE icraa FROM zhrbr_t006 INTO l_icratnm
                            WHERE icrad EQ gs_9950-icrad.
    l_icrano = gs_9950-dosno.
    l_sirano = gs_9950-kessr.
    SHIFT l_sirano LEFT DELETING LEADING '0'.
    CONCATENATE l_context l_sirano text-002
                                  INTO l_context SEPARATED BY space.
    CONCATENATE l_context l_icratnm l_icrano text-003
                                  INTO l_context SEPARATED BY space.
    CLEAR : l_icratnm, l_icrano.
  ENDLOOP.

  LOOP AT gt_9950 INTO gs_9950.
    CLEAR ls_9950.
    MOVE-CORRESPONDING gs_9950 TO ls_9950.
    ls_9950-aborc = gs_9950-icrtr + gs_9950-faizt.
    CALL METHOD zhrbr_cl001=>odenen_borc
      EXPORTING
        icrid = gs_9950-icrid
        begda = sy-datum
        endda = sy-datum
      IMPORTING
        icodm = ls_9950-odmtr.
    ls_9950-bborc = ls_9950-aborc - ls_9950-odmtr.
    APPEND ls_9950 TO lt_9950.
  ENDLOOP.

  gs_report-icrad = p1_icrad.
*  gs_report-icryr = p1_icryr.
  gs_report-dosno = p1_dosno.
*  gs_report-yonad = p1_yonad.
*  gs_report-yonun = p1_yonun.

  PERFORM set_date USING p1_evtrh CHANGING gs_report-evtrh.
  PERFORM set_date USING sy-datum CHANGING gs_report-daily.

  IF p_dosno IS NOT INITIAL .
    READ TABLE lt_9950 INTO ls_9950 WITH KEY dosno = p_dosno .
    DELETE lt_9950 WHERE kessr GT ls_9950-kessr .
*    DELETE lt_9014 WHERE zhr_ic_no LT ls_9014-zhr_ic_no .
  ENDIF .

  SORT lt_9950 BY icrid ."logid .
  LOOP AT lt_9950 INTO ls_9950 WHERE compl NE 'D'
                                 AND dosno NE p_dosno.
    CLEAR : lv_field ,l_sira, l_icrano,l_sumic, lv_counter, l_icratnm.

*    l_tabix = sy-tabix.
    l_tabix = l_tabix + 1 .
    SHIFT l_tabix LEFT DELETING LEADING '0'.
    SHIFT ls_9950-icrid  LEFT DELETING LEADING '0'.

    SELECT SINGLE icraa FROM zhrbr_t006 INTO l_icratnm
                            WHERE icrad EQ ls_9950-icrad.
    l_icrano = ls_9950-dosno.
    WRITE ls_9950-bborc TO l_sumic.
*    l_sumic  = ls_9014-zhr_sum_ickl ."ls_9014-zhr_sum_ic.

    CONCATENATE l_tabix text-002 INTO l_sira.
    CONCATENATE l_icratnm text-005 INTO l_icratnm.
    CONCATENATE l_sira l_icratnm l_icrano  text-004
                l_sumic 'TL' INTO lv_field SEPARATED BY space.
    CONDENSE lv_field.
    CONCATENATE '<<CONTENT' l_tabix '>>' INTO lv_counter.
    gs_word-name  = lv_counter.
    gs_word-value = lv_field.
    APPEND gs_word TO gt_word.

  ENDLOOP.

  DO.
    IF l_tabix LE 30.
      l_tabix = l_tabix + 1.
      CONCATENATE '<<CONTENT' l_tabix '>>' INTO lv_counter.
      gs_word-name  = lv_counter.
      gs_word-value = ' '.
      APPEND gs_word TO gt_word.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  gs_report-icrad = gs_9950-icrad.
  PERFORM icra_dairesi USING gs_9950-icrad
                    CHANGING gs_report-icraa
                             gs_report-icryr.

  SHIFT gs_report-dosno LEFT DELETING LEADING '0'.
  gs_word-name = '<<LOGID>>'.
  gs_word-value = sy-datum(4) . "gs_report-dosno.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TARIH>>'.
  gs_word-value = gs_report-daily.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ICRADAIRESI>>'.
  gs_word-value = gs_report-icraa.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<IL>>'.
  gs_word-value = gs_report-icryr.
  APPEND gs_word TO gt_word.

*  gs_word-name = '<<ADRES>>'.
*  gs_word-value = p1_icryr.
*  APPEND gs_word TO gt_word.

*  gs_word-name = '<<ADRES2>>'.
*  gs_word-value = p1_icry2.
*  APPEND gs_word TO gt_word.

*  gs_word-name = '<<TELEFON>>'.
*  gs_word-value = p1_usrid.
*  APPEND gs_word TO gt_word.


  gs_word-name = '<<EVRAKTARIHI>>'.
  gs_word-value = gs_report-evtrh.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<DOSYANO>>'.
  gs_word-value = gs_report-dosno.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<BUTXT>>'.
  gs_word-value = gs_report-butxt.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TCNO>>'.
  gs_word-value = gs_report-merni.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<AD>>'.
  gs_word-value = gs_report-ename.
  APPEND gs_word TO gt_word.

  DATA : lv_yon1 TYPE emnam,
         lv_yon2 TYPE emnam,
         lv_pln1 TYPE plans,
         lv_pln2 TYPE plans,
         lv_unv1 TYPE stext,
         lv_unv2 TYPE stext,
         lv_bukr TYPE bukrs,
         lv_butx TYPE butxt.

  SELECT SINGLE  bukrs FROM pa0001 INTO lv_bukr
                     WHERE pernr EQ p_pernr
                       AND begda LE sy-datum
                       AND endda GE sy-datum .
  SELECT SINGLE butxt  FROM t001   INTO lv_butx
                     WHERE bukrs EQ lv_bukr.


  gs_word-name = '<<BUKRS>>'.
  gs_word-value = lv_butx ."gs_report-yonad.
  APPEND gs_word TO gt_word.

  SELECT SINGLE ename plans FROM pa0001 INTO (lv_yon1 , lv_pln1)
                     WHERE pernr EQ p4_imza1
                       AND begda LE sy-datum
                       AND endda GE sy-datum .
*
  SELECT SINGLE stext FROM hrp1000 INTO lv_unv1
                      WHERE otype EQ 'S'
                        AND objid EQ lv_pln1
                        AND langu EQ sy-langu
                        AND begda LE sy-datum
                        AND endda GE sy-datum .
*
*
  gs_word-name = '<<IMZA>>'.
  gs_word-value = lv_yon1 ."gs_report-yonad.
  APPEND gs_word TO gt_word.
*
  gs_word-name = '<<UNVAN>>'.
  gs_word-value = lv_unv1 ."gs_report-yonun.
  APPEND gs_word TO gt_word.

  SELECT SINGLE ename plans FROM pa0001 INTO (lv_yon2 , lv_pln2)
                   WHERE pernr EQ p1_imza2
                     AND begda LE sy-datum
                     AND endda GE sy-datum .

  SELECT SINGLE stext FROM hrp1000 INTO lv_unv2
                      WHERE otype EQ 'S'
                        AND objid EQ lv_pln2
                        AND langu EQ sy-langu
                        AND begda LE sy-datum
                        AND endda GE sy-datum .

  gs_word-name = '<<IMZA2>>'.
  gs_word-value = lv_yon2 .
  APPEND gs_word TO gt_word.

  gs_word-name = '<<UNVAN2>>'.
  gs_word-value = lv_unv2 .
  APPEND gs_word TO gt_word.

  gs_word-name = '<<CONTENT>>'.
  gs_word-value = l_context.
  APPEND gs_word TO gt_word.
  DATA : lv_cnt       TYPE i, lv_text(255), lv_say(20).
  CLEAR : lv_text ,lv_cnt, lv_say.
*  DESCRIBE TABLE lt_9014 LINES lv_cnt .
  LOOP AT lt_9950 INTO ls_9950 WHERE compl NE 'D'.
    lv_cnt = lv_cnt + 1.
  ENDLOOP.
  lv_tabx = lv_cnt .
  gs_word-name = '<<ICNO>>'.
  gs_word-value = lv_tabx.
  APPEND gs_word TO gt_word.

  SUBTRACT 1 FROM lv_tabx .
  IF lv_tabx IS NOT INITIAL .
    IF lv_tabx EQ 1.
      lv_say = 'Birinci'.
    ELSEIF lv_tabx EQ 2.
      lv_say = 'İkinci'.
    ELSEIF lv_tabx EQ 3.
      lv_say = 'Üçüncü'.
    ELSEIF lv_tabx EQ 4.
      lv_say = 'Dördüncü'.
    ELSEIF lv_tabx EQ 5.
      lv_say = 'Beşinci'.
    ELSEIF lv_tabx EQ 6.
      lv_say = 'Altıncı'.
    ELSEIF lv_tabx EQ 7.
      lv_say = 'Yedinci'.
    ELSEIF lv_tabx EQ 8.
      lv_say = 'Sekizinci'.
    ELSEIF lv_tabx EQ 9.
      lv_say = 'Dokuzuncu'.
    ELSEIF lv_tabx EQ 10.
      lv_say = 'Onuncu'.
    ELSEIF lv_tabx EQ 11.
      lv_say = 'Onbirinci'.
    ELSEIF lv_tabx EQ 12.
      lv_say = 'Onikinci'.
    ELSEIF lv_tabx EQ 13.
      lv_say = 'Onüçüncü'.
    ELSEIF lv_tabx EQ 14.
      lv_say = 'Ondördüncü'.
    ELSEIF lv_tabx EQ 15.
      lv_say = 'Onbeşinci'.
    ELSEIF lv_tabx EQ 16.
      lv_say = 'Onaltıncı'.
    ELSEIF lv_tabx EQ 17.
      lv_say = 'Onyedinci'.
    ELSEIF lv_tabx EQ 18.
      lv_say = 'Onsekizinci'.
    ELSEIF lv_tabx EQ 19.
      lv_say = 'Ondokuzuncu'.
    ELSEIF lv_tabx EQ 20.
      lv_say = 'Yirminci'.
    ELSEIF lv_tabx EQ 21.
      lv_say = 'Yirmibirinci'.
    ELSEIF lv_tabx EQ 22.
      lv_say = 'Yirmiikinci'.
    ELSEIF lv_tabx EQ 23.
      lv_say = 'Yirmiüçüncü'.
    ELSEIF lv_tabx EQ 24.
      lv_say = 'Yirmidördüncü'.
    ELSEIF lv_tabx EQ 25.
      lv_say = 'Yirmibeşinci'.
    ELSEIF lv_tabx EQ 26.
      lv_say = 'Yirmialtıncı'.
    ELSEIF lv_tabx EQ 27.
      lv_say = 'Yirmiyedinci'.
    ELSEIF lv_tabx EQ 28.
      lv_say = 'Yirmisekizinci'.
    ELSEIF lv_tabx EQ 29.
      lv_say = 'Yirmidokuzuncu'.
    ELSEIF lv_tabx EQ 30.
      lv_say = 'Otuzuncu'.
    ENDIF.

    IF lv_say IS NOT INITIAL.
      CONCATENATE lv_say 'sıradaki borcu bittikten sonra Müdürlüğünüzde'
                  'işlem gören dosyaya ödeme yapılmaya başlanacaktır.'
             INTO lv_text SEPARATED BY space .
    ENDIF.

  ENDIF.


  gs_word-name = '<<ICNO1>>'.
  gs_word-value = lv_text .                                 "lv_tabx1.
  APPEND gs_word TO gt_word.

  CALL FUNCTION 'ZHRBR_FG001_001'
    EXPORTING
      iv_classname  = 'ZHRBR'
      iv_classtype  = 'OT'
      iv_object_key = 'ICRA-10'
    TABLES
      t_value       = gt_word.

ENDFORM.                                                    "set_110
*&---------------------------------------------------------------------*
*&      Form  SET_120
*&---------------------------------------------------------------------*
FORM set_120 .
  DATA: l_donem(20).
  DATA : l_sumic(15) TYPE c.
  DATA lv_ay TYPE numc2.
  DATA ls_247 TYPE t247.
  SELECT * FROM pa9950 INTO TABLE gt_9950 WHERE pernr EQ gs_report-pernr.

*  READ TABLE gt_9014 INTO gs_9014 WITH KEY zhr_ic_act = '1'.
  READ TABLE gt_9950 INTO gs_9950 WITH KEY dosno = gs_report-dosno.
  CHECK sy-subrc EQ 0.
  PERFORM set_date USING sy-datum              CHANGING gs_report-daily.
*  PERFORM set_date USING gs_9014-zhr_ic_kestrh CHANGING gs_report-evtrh.
  PERFORM set_date USING sy-datum CHANGING gs_report-evtrh.

  CLEAR: lv_ay, ls_247, l_donem.
  lv_ay = sy-datum+4(2).
  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      langu = sy-langu
      month = lv_ay
    IMPORTING
      t247  = ls_247.
  IF sy-subrc EQ 0.
    CONCATENATE ls_247-ltx '/' sy-datum+0(4)
    INTO l_donem SEPARATED BY space.
  ENDIF.

  gs_report-icrad = gs_9950-icrad.
  PERFORM icra_dairesi USING gs_9950-icrad
                    CHANGING gs_report-icraa
                             gs_report-icryr.

  gs_report-dosno = p2_kytno ."gs_9014-zhr_ic_dosno.
*  gs_report-yonad = p2_yonad.
*  gs_report-yonun = p2_yonun.
  gs_report-kytno = p2_kytno.

  gs_word-name = '<<DONEM>>'.
  gs_word-value = l_donem.
  APPEND gs_word TO gt_word.

  gs_9950-icrtr = gs_9950-icrtr + gs_9950-faizt."Toplam Borç
  WRITE gs_9950-icrtr TO l_sumic.
  CONDENSE l_sumic.
  gs_word-name = '<<SUMIC>>'.
  gs_word-value = l_sumic.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TARIH>>'.
  gs_word-value = gs_report-daily.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ICRADAIRESI>>'.
  gs_word-value = gs_report-icraa.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<IL>>'.
  gs_word-value = gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADRES>>'.
  gs_word-value = gs_report-adr . "gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADRES2>>'.
  gs_word-value = gs_report-adr2 . "gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TELEFON>>'.
  gs_word-value = gs_report-tel ."gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<SAYI>>'.
  gs_word-value = sy-datum(4) ."gs_report-kytno.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<EVRAKTARIHI>>'.
*  gs_word-value = p2_evtrh . "gs_report-evtrh.
  WRITE p2_evtrh TO gs_word-value.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<DOSYANO>>'.
  gs_word-value = gs_report-dosno.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<SIRKETADI>>'.
  gs_word-value = gs_report-butxt.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TCNO>>'.
  gs_word-value = gs_report-merni.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADSOYAD>>'.
  gs_word-value = gs_report-ename.
  APPEND gs_word TO gt_word.

  DATA : lv_yon1 TYPE emnam,
         lv_yon2 TYPE emnam,
         lv_pln1 TYPE plans,
         lv_pln2 TYPE plans,
         lv_unv1 TYPE stext,
         lv_unv2 TYPE stext.

  SELECT SINGLE ename plans FROM pa0001 INTO (lv_yon1 , lv_pln1)
                     WHERE pernr EQ p2_imza1
                       AND begda LE sy-datum
                       AND endda GE sy-datum .

  SELECT SINGLE stext FROM hrp1000 INTO lv_unv1
                      WHERE otype EQ 'S'
                        AND objid EQ lv_pln1
                        AND langu EQ sy-langu
                        AND begda LE sy-datum
                        AND endda GE sy-datum .

  gs_word-name = '<<IMZA>>'.
  gs_word-value = lv_yon1 ."gs_report-yonad.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<UNVAN>>'.
  gs_word-value = lv_unv1 ."gs_report-yonun.
  APPEND gs_word TO gt_word.

  SELECT SINGLE ename plans FROM pa0001 INTO (lv_yon2 , lv_pln2)
                   WHERE pernr EQ p2_imza2
                     AND begda LE sy-datum
                     AND endda GE sy-datum .

  SELECT SINGLE stext FROM hrp1000 INTO lv_unv2
                      WHERE otype EQ 'S'
                        AND objid EQ lv_pln2
                        AND langu EQ sy-langu
                        AND begda LE sy-datum
                        AND endda GE sy-datum .

  gs_word-name = '<<IMZA2>>'.
  gs_word-value = lv_yon2 .
  APPEND gs_word TO gt_word.

  gs_word-name = '<<UNVAN2>>'.
  gs_word-value = lv_unv2 .
  APPEND gs_word TO gt_word.

  DATA:  lv_bukr TYPE bukrs,
         lv_butx TYPE butxt.

  SELECT SINGLE  bukrs FROM pa0001 INTO lv_bukr
                     WHERE pernr EQ p_pernr
                       AND begda LE sy-datum
                       AND endda GE sy-datum .
  SELECT SINGLE butxt  FROM t001   INTO lv_butx
                     WHERE bukrs EQ lv_bukr.

  gs_word-name = '<<BUKRS>>'.
  gs_word-value = lv_butx ."gs_report-yonad.
  APPEND gs_word TO gt_word.


  CALL FUNCTION 'ZHRBR_FG001_001'
    EXPORTING
      iv_classname  = 'ZHRBR'
      iv_classtype  = 'OT'
      iv_object_key = 'ICRA-20'
    TABLES
      t_value       = gt_word.
ENDFORM.                                                    "set_120
*&---------------------------------------------------------------------*
*&      Form  SET_130
*&---------------------------------------------------------------------*
FORM set_130 .
  DATA : l_endda TYPE datum,
         l_cikis TYPE zhrbr_de032.
  SELECT SINGLE begda FROM pa0000 INTO l_endda
                                 WHERE pernr EQ gs_report-pernr
                                   AND massn EQ '10'
                                   AND endda EQ '99991231'.
  IF l_endda IS INITIAL.
    l_endda = sy-datum.
  ELSE.
    l_endda = l_endda - 1.
  ENDIF.

  SELECT * FROM pa9950 INTO TABLE gt_9950 WHERE pernr EQ gs_report-pernr.

  PERFORM set_date USING sy-datum CHANGING gs_report-daily.
  PERFORM set_date USING l_endda  CHANGING l_cikis.



*  gs_report-yonad = p3_yonad.
*  gs_report-yonun = p3_yonun.
  gs_report-kytno = p3_kytno.

  READ TABLE gt_9950 INTO gs_9950 INDEX 1.
*  LOOP AT gt_9014 INTO gs_9014.
  CLEAR : gs_report-evtrh, gs_report-evtrh, gs_report-icrad,
          gs_report-icryr, gs_report-dosno, gt_word, gs_word .

  PERFORM set_date USING sy-datum" BELLİ OLUNCA KONUALACAK
                CHANGING gs_report-evtrh.

  gs_report-icrad = gs_9950-icrad.
  PERFORM icra_dairesi USING gs_9950-icrad
                    CHANGING gs_report-icraa
                             gs_report-icryr.

  gs_report-dosno = gs_9950-dosno.

  gs_word-name = '<<CIKISTARIHI>>'.
  gs_word-value = l_cikis.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TARIH>>'.
  gs_word-value = gs_report-daily.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ICRADAIRESI>>'.
  gs_word-value = gs_report-icraa.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<IL>>'.
  gs_word-value = gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADRES>>'.
  gs_word-value = gs_report-adr . "gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADRES2>>'.
  gs_word-value = gs_report-adr2 . "gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TELEFON>>'.
  gs_word-value = gs_report-tel ."gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<SAYI>>'.
  gs_word-value = sy-datum(4) ."gs_report-kytno.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<EVRAKTARIHI>>'.
*    gs_word-value = p3_evtrh ."gs_report-evtrh.
  WRITE p3_evtrh TO gs_word-value.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<DOSYANO>>'.
  gs_word-value = gs_report-dosno.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<SIRKETADI>>'.
  gs_word-value = gs_report-butxt.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TCNO>>'.
  gs_word-value = gs_report-merni.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADSOYAD>>'.
  gs_word-value = gs_report-ename.
  APPEND gs_word TO gt_word.

  DATA : lv_yon1 TYPE emnam,
         lv_yon2 TYPE emnam,
         lv_pln1 TYPE plans,
         lv_pln2 TYPE plans,
         lv_unv1 TYPE stext,
         lv_unv2 TYPE stext.

  SELECT SINGLE ename plans FROM pa0001 INTO (lv_yon1 , lv_pln1)
                     WHERE pernr EQ p3_imza1
                       AND begda LE sy-datum
                       AND endda GE sy-datum .

  SELECT SINGLE stext FROM hrp1000 INTO lv_unv1
                      WHERE otype EQ 'S'
                        AND objid EQ lv_pln1
                        AND langu EQ sy-langu
                        AND begda LE sy-datum
                        AND endda GE sy-datum .

  gs_word-name = '<<IMZA>>'.
  gs_word-value = lv_yon1 ."gs_report-yonad.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<UNVAN>>'.
  gs_word-value = lv_unv1 ."gs_report-yonun.
  APPEND gs_word TO gt_word.

  SELECT SINGLE ename plans FROM pa0001 INTO (lv_yon2 , lv_pln2)
                     WHERE pernr EQ p3_imza2
                       AND begda LE sy-datum
                       AND endda GE sy-datum .

  SELECT SINGLE stext FROM hrp1000 INTO lv_unv2
                      WHERE otype EQ 'S'
                        AND objid EQ lv_pln2
                        AND langu EQ sy-langu
                        AND begda LE sy-datum
                        AND endda GE sy-datum .

  gs_word-name = '<<IMZA2>>'.
  gs_word-value = lv_yon2 ."gs_report-yonad.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<UNVAN2>>'.
  gs_word-value = lv_unv2 ."gs_report-yonun.
  APPEND gs_word TO gt_word.
  DATA:  lv_bukr TYPE bukrs,
         lv_butx TYPE butxt.

  SELECT SINGLE  bukrs FROM pa0001 INTO lv_bukr
                     WHERE pernr EQ p_pernr
                       AND begda LE sy-datum
                       AND endda GE sy-datum .
  SELECT SINGLE butxt  FROM t001   INTO lv_butx
                     WHERE bukrs EQ lv_bukr.

  gs_word-name = '<<BUKRS>>'.
  gs_word-value = lv_butx ."gs_report-yonad.
  APPEND gs_word TO gt_word.


  CALL FUNCTION 'ZHRBR_FG001_001'
    EXPORTING
      iv_classname  = 'ZHRBR'
      iv_classtype  = 'OT'
      iv_object_key = 'ICRA-30'
    TABLES
      t_value       = gt_word.
*  ENDLOOP.
ENDFORM.                                                    "set_130
*&---------------------------------------------------------------------*
*&      Form  SET_140
*&---------------------------------------------------------------------*
FORM set_140 .
  DATA: l_donem(20).
  DATA : l_sumic(15) TYPE c,
         lv_sumic    TYPE betrg.
  DATA lv_ay TYPE numc2.
  DATA ls_247 TYPE t247.
  DATA lt_9950 TYPE TABLE OF p9950.
  DATA ls_9950 TYPE p9950.
  DATA lt_9951 TYPE TABLE OF p9951.
  DATA ls_9951 TYPE p9951.
  PERFORM set_date USING sy-datum CHANGING gs_report-daily.
  PERFORM set_date USING p4_evtrh CHANGING gs_report-evtrh.


  SELECT  * FROM pa9950 INTO CORRESPONDING FIELDS OF TABLE lt_9950
                                            WHERE pernr EQ gs_report-pernr
                                              AND dosno  EQ p4_dosno.
  SORT lt_9950 DESCENDING BY begda.
  READ TABLE lt_9950 INTO ls_9950 INDEX 1.

  SELECT  * FROM pa9951 INTO CORRESPONDING FIELDS OF TABLE lt_9951
                                           WHERE pernr EQ ls_9950-pernr
                                             AND icrid EQ ls_9950-icrid.
  SORT lt_9951 DESCENDING BY begda.
  READ TABLE lt_9951 INTO ls_9951 INDEX 1.
*  DEBTC
  CLEAR: lv_ay, ls_247, l_donem.
*  lv_ay = sy-datum+4(2).
  lv_ay = ls_9950-begda+4(2).
  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      langu = sy-langu
      month = lv_ay
    IMPORTING
      t247  = ls_247.
  IF sy-subrc EQ 0.
    CONCATENATE ls_247-ltx '/' ls_9951-begda+0(4)
    INTO l_donem SEPARATED BY space.
  ENDIF.

*  SELECT SINGLE zhr_sum_ic FROM pa9014
*                           INTO lv_sumic
  DATA s9950 TYPE pa9950.
  SELECT SINGLE * FROM pa9950 INTO s9950
                             WHERE pernr    EQ gs_report-pernr
                               AND compl    NE 'D'
*                               AND zhr_ic_kestrh EQ p4_evtrh
                               AND dosno    EQ p4_dosno.

  DATA l_fired TYPE p0000-begda.
  SELECT SINGLE begda FROM pa0000 INTO l_fired
    WHERE pernr EQ gs_report-pernr
      AND endda GE sy-datum
      AND begda LE sy-datum
      AND massn EQ '10'.
  IF sy-subrc EQ 0.
    l_fired = l_fired - 1.
  ELSE.
    l_fired = sy-datum."İşten çıkmamış olan personel için
  ENDIF.


*  gs_report-icraa = p4_icrad.
*  gs_report-icryr = p4_icryr.
  gs_report-dosno = p4_dosno.
  gs_report-icrad = gs_9950-icrad.

*  gs_report-yonad = p4_yonad.
*  gs_report-yonun = p4_yonun.
*  gs_report-kytno = p4_kytno.

  gs_word-name = '<<DONEM>>'.
  gs_word-value = l_donem.
  APPEND gs_word TO gt_word.

*  l_sumic = lv_sumic.
  s9950-icrtr = s9950-icrtr + s9950-faizt."Toplam Borç
  WRITE s9950-icrtr TO l_sumic.
*  l_sumic = s9014-zhr_sum_ic.
  CONDENSE l_sumic.
  gs_word-name = '<<SUMIC>>'.
  gs_word-value = l_sumic.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TARIH>>'.
  gs_word-value = gs_report-daily.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ICRADAIRESI>>'.
  gs_word-value = gs_report-icraa.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<IL>>'.
  gs_word-value = gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADRES>>'.
  gs_word-value = gs_report-adr . "gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADRES2>>'.
  gs_word-value = gs_report-adr2 . "gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TELEFON>>'.
  gs_word-value = gs_report-tel ."gs_report-icryr.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<SAYI>>'.
  gs_word-value = sy-datum(4) ."gs_report-kytno.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<EVRAKTARIHI>>'.
*  gs_word-value = gs_report-evtrh.
  WRITE gs_report-evtrh TO gs_word-value.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<DOSYANO>>'.
  gs_word-value = gs_report-dosno.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<SIRKETADI>>'.
  gs_word-value = gs_report-butxt.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<TCNO>>'.
  gs_word-value = gs_report-merni.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<ADSOYAD>>'.
  gs_word-value = gs_report-ename.
  APPEND gs_word TO gt_word.

  DATA l_firedx(10).
  WRITE l_fired TO l_firedx USING EDIT MASK '__.__.____'.
  gs_word-name = '<<FIRED>>'.
  gs_word-value = l_firedx.
  APPEND gs_word TO gt_word.

  DATA dl TYPE char50.
  DATA dl2 TYPE char50.
  DATA lv_dec TYPE betrg. "Aylık Kesilen
  DATA: lv_odmtr TYPE betrg.
  CLEAR: lv_dec, lv_odmtr.
  lv_dec = ls_9951-odmtr.
  WRITE lv_dec TO dl.
  gs_word-name = '<<KESILEN>>'.
  gs_word-value = dl.
  CONDENSE gs_word-value NO-GAPS.
  APPEND gs_word TO gt_word.

  CLEAR: dl, dl2,lv_dec.
*  dl2 = s9014-zhr_sum_ic - s9014-zhr_sum_ickl.
  CLEAR: lv_dec, lv_odmtr.
  CALL METHOD zhrbr_cl001=>odenen_borc
    EXPORTING
      icrid = ls_9950-icrid
      begda = sy-datum
      endda = sy-datum
    IMPORTING
      icodm = lv_odmtr.
  lv_dec = s9950-icrtr - lv_odmtr.
*  dl2 = s_9950-icrtr - lv_odmtr.
*  lv_dec = dl2.
*  WRITE dl2 TO dl.
  WRITE lv_dec TO dl.
  gs_word-name = '<<TOPKESILEN>>'.
  gs_word-value = dl.
  CONDENSE gs_word-value NO-GAPS.
  APPEND gs_word TO gt_word.

  CLEAR: dl, dl2, lv_dec.
  lv_dec = s9950-icrtr - lv_odmtr.
*  WRITE s9014-zhr_sum_ickl TO dl.
  gs_word-name = '<<KALANICRA>>'.
  gs_word-value = dl.
  CONDENSE gs_word-value NO-GAPS .
  APPEND gs_word TO gt_word.

  DATA : lv_yon1 TYPE emnam,
         lv_yon2 TYPE emnam,
         lv_pln1 TYPE plans,
         lv_pln2 TYPE plans,
         lv_unv1 TYPE stext,
         lv_unv2 TYPE stext.

  SELECT SINGLE ename plans FROM pa0001 INTO (lv_yon1 , lv_pln1)
                     WHERE pernr EQ p4_imza1
                       AND begda LE sy-datum
                       AND endda GE sy-datum .
*
  SELECT SINGLE stext FROM hrp1000 INTO lv_unv1
                      WHERE otype EQ 'S'
                        AND objid EQ lv_pln1
                        AND langu EQ sy-langu
                        AND begda LE sy-datum
                        AND endda GE sy-datum .
*
*
  gs_word-name = '<<IMZA>>'.
  gs_word-value = lv_yon1 ."gs_report-yonad.
  APPEND gs_word TO gt_word.
*
  gs_word-name = '<<UNVAN>>'.
  gs_word-value = lv_unv1 ."gs_report-yonun.
  APPEND gs_word TO gt_word.

  SELECT SINGLE ename plans FROM pa0001 INTO (lv_yon2 , lv_pln2)
                     WHERE pernr EQ p4_imza2
                       AND begda LE sy-datum
                       AND endda GE sy-datum .

  SELECT SINGLE stext FROM hrp1000 INTO lv_unv2
                      WHERE otype EQ 'S'
                        AND objid EQ lv_pln2
                        AND langu EQ sy-langu
                        AND begda LE sy-datum
                        AND endda GE sy-datum .


  gs_word-name = '<<IMZA2>>'.
  gs_word-value = lv_yon2 ."gs_report-yonad.
  APPEND gs_word TO gt_word.

  gs_word-name = '<<UNVAN2>>'.
  gs_word-value = lv_unv2 ."gs_report-yonun.
  APPEND gs_word TO gt_word.
  DATA:  lv_bukr TYPE bukrs,
         lv_butx TYPE butxt.

  SELECT SINGLE  bukrs FROM pa0001 INTO lv_bukr
                     WHERE pernr EQ p_pernr
                       AND begda LE sy-datum
                       AND endda GE sy-datum .
  SELECT SINGLE butxt  FROM t001   INTO lv_butx
                     WHERE bukrs EQ lv_bukr.

  gs_word-name = '<<BUKRS>>'.
  gs_word-value = lv_butx ."gs_report-yonad.
  APPEND gs_word TO gt_word.


  CALL FUNCTION 'ZHRBR_FG001_001'
    EXPORTING
      iv_classname  = 'ZHRBR'
      iv_classtype  = 'OT'
      iv_object_key = 'ICRA-40'
    TABLES
      t_value       = gt_word.
ENDFORM.                                                    "set_140
*&---------------------------------------------------------------------*
*&      Form  CONTROL
*&---------------------------------------------------------------------*
FORM control .
  DATA : lv_numb(4),
         lv_temp(3),
         lv_date(10),
         lv_alert  .

  DATA : lt_fields TYPE TABLE OF d021s,
         ls_fields TYPE d021s.

  FIELD-SYMBOLS <fs> TYPE any.

  CONCATENATE 'P' number+1(1) '_' INTO lv_temp.
  CONCATENATE '0' number INTO lv_numb.

  CALL FUNCTION 'IAC_GET_DYNPRO_INFO'
    EXPORTING
      program = sy-repid
      dynpro  = lv_numb
    TABLES
      fields  = lt_fields.

  LOOP AT lt_fields INTO ls_fields.
    IF ls_fields-fnam+0(3) EQ lv_temp.
      ASSIGN (ls_fields-fnam) TO <fs>.
      CHECK <fs> IS ASSIGNED .
      IF <fs> IS INITIAL.
        IF ls_fields-fnam CS 'ICRYR' OR
           ls_fields-fnam CS 'ICRY2'.
          CLEAR lv_alert.
        ELSE.
          lv_alert = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CHECK lv_alert IS NOT INITIAL.
  MESSAGE text-e01 TYPE 'I' DISPLAY LIKE 'E'.
  gv_error = abap_true.
ENDFORM.                    "control
*&---------------------------------------------------------------------*
*&      Form  ICRA_DAIRESI
*&---------------------------------------------------------------------*
FORM icra_dairesi  USING    p_icrad TYPE zhrbr_de020
                   CHANGING p_icraa p_icryr.
  SELECT SINGLE icraa icryr FROM zhrbr_t006
                                      INTO (p_icraa,p_icryr)
                                      WHERE icrad = p_icrad.
ENDFORM.                    "icra_dairesi
