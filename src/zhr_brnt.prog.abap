*&---------------------------------------------------------------------*
*&  Include           ZHR_BRNT
*&---------------------------------------------------------------------*

FORM  fuzbrnt.

  CASE as-parm1.
    WHEN '01'. PERFORM brutten_nete_detay.
  ENDCASE.


ENDFORM.                    "FUZBRNT
*&---------------------------------------------------------------------*
*& Form brutten_nete_detay
*&---------------------------------------------------------------------*
FORM brutten_nete_detay .

  DATA t_512w LIKE STANDARD TABLE OF t512w WITH HEADER LINE .
  DATA : lt_df LIKE TABLE OF rt WITH HEADER LINE .
  CONSTANTS: v_cum01 TYPE numc2 VALUE '01'.
  CONSTANTS: v_cum02 TYPE numc2 VALUE '02'.
  CONSTANTS: v_cum03 TYPE numc2 VALUE '03'.
  CONSTANTS: v_cum04 TYPE numc2 VALUE '04'.
  CONSTANTS: v_cum11 TYPE numc2 VALUE '11'.
  DATA: v_bit TYPE c.
  DATA lt_t052 TYPE TABLE OF /dsl/hr20_t053.
  DATA ls_t052 TYPE /dsl/hr20_t053.

  RANGES: r_lga FOR t512t-lgart.

  TABLES: /dsl/hr20_t053 .

  DATA: lv_pernr          TYPE p_pernr,
        lv_period         TYPE paper,
        lv_ssk_matrah     TYPE maxbt,
        lv_iss_matrah     TYPE maxbt,
        lv_glr_matrah     TYPE maxbt,
        lv_dmg_matrah     TYPE maxbt,
        gv_mat            TYPE maxbt,
        gv_kes            TYPE maxbt,
        gv_dmg            TYPE maxbt,
        lv_gv             TYPE maxbt,
        kumule_gv         TYPE maxbt,
        kumule_ssk        TYPE maxbt,
        lv_ssk_matrah_hes TYPE maxbt,
        lv_iss_matrah_hes TYPE maxbt,
        lv_glr_matrah_hes TYPE maxbt,
        lv_dmg_matrah_hes TYPE maxbt,
        lv_ssk_hes        TYPE maxbt,
        lv_fark           TYPE maxbt,
        lv_ssk_gun_hes    TYPE maxbt,
        lv_ssk_tavan_gun  TYPE maxbt,
        lv_ssk_tavan      TYPE maxbt,
        lv_iss_hes        TYPE maxbt,
        lv_glr_hes        TYPE maxbt,
        lv_dmg_hes        TYPE maxbt,
        lv_gmuaf          TYPE maxbt,
        lv_dmuaf          TYPE maxbt,
        lv_muafd          TYPE maxbt,
        lv_kdmgm          TYPE maxbt,
        lv_kglrm          TYPE maxbt,
        lv_ssk            TYPE maxbt,
        lv_dmg            TYPE maxbt,
        lv_dmmmt          TYPE maxbt,
        lv_frbmt          TYPE maxbt,
        lv_sskmt          TYPE maxbt,
        lv_kumgm          TYPE maxbt,
        lv_devreden       TYPE maxbt,
        lv_ssk_rate       TYPE ptr_senee,
        lv_iss_rate       TYPE ptr_senee,
        lv_glr_rate       TYPE ptr_prznt,
        lv_dmg_rate       TYPE ptr_stamp.
  DATA: lt_dilim TYPE TABLE OF t7trt01 WITH HEADER LINE.
  DATA ls_rt LIKE rt .
  DATA ls_rt1 LIKE rt .
  DATA lv_rt LIKE rt .

  SELECT * FROM /dsl/hr20_t053 INTO TABLE lt_t052.

  SORT lt_t052 ASCENDING BY fbrut.

  LOOP AT lt_t052 INTO ls_t052.
    r_lga-sign = 'I'.
    r_lga-option = 'EQ'.
    r_lga-low = ls_t052-fbrut.
    APPEND r_lga. CLEAR r_lga.
  ENDLOOP.
  REFRESH lt_df.
  LOOP AT wpbp.
    LOOP AT rt WHERE lgart EQ '/DMM' OR lgart EQ '/FRB'.
      MOVE-CORRESPONDING rt TO lt_df.
      lt_df-betrg = ( ( lt_df-betrg / 30 ) * wpbp-asoll ).
      lt_df-apznr = wpbp-apznr.
      COLLECT lt_df.
    ENDLOOP.
  ENDLOOP.

  READ TABLE rt WITH KEY lgart = '/DMT' .
  IF sy-subrc = 0 .
    lv_devreden = rt-betrg .
  ENDIF.

  READ TABLE crt WITH KEY lgart = '/102' .
  IF sy-subrc = 0 .
    kumule_gv = crt-betrg.
  ELSE.
    CLEAR kumule_gv.
  ENDIF.

  LOOP AT wpbp .
    CLEAR: kumule_ssk, lv_ssk_hes,
           lv_iss_hes,lv_glr_hes,
           lv_dmg_hes,lv_ssk_matrah_hes,lv_ssk_gun_hes, lv_devreden,
           lv_ssk_rate,lv_iss_rate, lv_gmuaf, lv_dmuaf, lv_kdmgm,
lv_kglrm.
    READ TABLE rt WITH KEY lgart = '/NEE'
                           apznr = wpbp-apznr .
    IF sy-subrc = 0 .
      lv_ssk_hes = rt-betrg .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/IEE'
                           apznr = wpbp-apznr.
    IF sy-subrc = 0 .
      lv_iss_hes = rt-betrg .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/SAN'
                           apznr = wpbp-apznr .
    IF sy-subrc = 0 .
      lv_glr_hes = rt-betrg .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/S01'
                           apznr = wpbp-apznr .
    IF sy-subrc = 0 .
      lv_dmg_hes = rt-betrg .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/104'
                           apznr = wpbp-apznr.
    IF sy-subrc = 0 .
      lv_ssk_matrah_hes = rt-betrg .
*    lv_ssk_gun_hes = rt-anzhl .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/NDY'
                           apznr = wpbp-apznr.
    IF sy-subrc = 0 .
      lv_ssk_gun_hes = rt-anzhl .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/PNE'
                           apznr = wpbp-apznr.
    IF sy-subrc = 0 .
      lv_ssk_rate = rt-anzhl / 100 .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/PIE'
                           apznr = wpbp-apznr.
    IF sy-subrc = 0 .
      lv_iss_rate = rt-anzhl / 100  .
    ENDIF.


    lv_dmg_rate = t7trp02-stamp.
    lv_ssk_tavan_gun = t7trs02-sskmx / 30 .
    lv_ssk_tavan = ( t7trs02-sskmx / 30 ) * lv_ssk_gun_hes .

    CLEAR lt_dilim. REFRESH lt_dilim.
    SELECT * FROM t7trt01
         INTO CORRESPONDING FIELDS OF TABLE lt_dilim
         WHERE grtax EQ t7trg04-grtax
*           AND begda LE pn-endda
*           AND endda GE pn-begda
           AND begda LE wpbp-endda
           AND endda GE wpbp-begda  .


    LOOP AT r_lga.


      CLEAR: gv_kes, gv_dmg .
      CLEAR /dsl/hr20_t053 .
      SELECT SINGLE * FROM /dsl/hr20_t053
                     WHERE fbrut = r_lga-low.
      CLEAR lv_rt.
      READ TABLE rt INTO lv_rt WITH KEY lgart = /dsl/hr20_t053-fnett
                                        apznr = wpbp-apznr.
      CHECK lv_rt-betrg IS INITIAL.
*  check /dsl/hr20_t053 is not initial .
*      IF /dsl/hr20_t053-fssk IS NOT INITIAL.
      IF kumule_ssk < lv_ssk_matrah_hes .
        READ TABLE rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
        IF sy-subrc = 0 .
          kumule_ssk = kumule_ssk + rt-betrg .
          IF kumule_ssk >= lv_ssk_matrah_hes .
            CLEAR ls_rt .
            READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
            lv_fark = kumule_ssk - rt-betrg .
            IF lv_fark < 0 .
              lv_fark = 0 .
            ENDIF.
            ls_rt-betrg = ( lv_ssk_matrah_hes - lv_fark ) *
                        lv_ssk_rate.
            gv_kes = gv_kes + ls_rt-betrg .
            IF /dsl/hr20_t053-fssk IS NOT INITIAL.
              ls_rt-lgart = /dsl/hr20_t053-fssk .

              CLEAR: ls_rt-betpe,ls_rt-anzhl.
              ls_rt-apznr = wpbp-apznr.
              COLLECT ls_rt INTO rt .
            ENDIF.

            CLEAR ls_rt .
            READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
            lv_fark = kumule_ssk - rt-betrg .
            IF lv_fark < 0 .
              lv_fark = 0 .
            ENDIF.
            ls_rt-betrg = ( lv_ssk_matrah_hes - lv_fark ) *
                            lv_iss_rate .
            gv_kes = gv_kes + ls_rt-betrg .
            IF /dsl/hr20_t053-fissz IS NOT INITIAL.
              ls_rt-lgart = /dsl/hr20_t053-fissz .

              CLEAR: ls_rt-betpe,ls_rt-anzhl.
              ls_rt-apznr = wpbp-apznr.
              COLLECT ls_rt INTO rt .
            ENDIF.
          ELSE.
            CLEAR ls_rt .
            READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
            ls_rt-betrg = ( rt-betrg + lv_devreden ) * lv_ssk_rate .
            gv_kes = gv_kes + ls_rt-betrg .
            IF /dsl/hr20_t053-fssk IS NOT INITIAL.
              ls_rt-lgart = /dsl/hr20_t053-fssk .

              CLEAR: ls_rt-betpe,ls_rt-anzhl.
              ls_rt-apznr = wpbp-apznr.
              COLLECT ls_rt INTO rt .
            ENDIF.

            CLEAR ls_rt .
            READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
            ls_rt-betrg = ( rt-betrg + lv_devreden ) * lv_iss_rate .
            lv_devreden = 0.
            gv_kes = gv_kes + ls_rt-betrg .
            IF /dsl/hr20_t053-fissz IS NOT INITIAL.
              ls_rt-lgart = /dsl/hr20_t053-fissz .

              CLEAR: ls_rt-betpe,ls_rt-anzhl.
              ls_rt-apznr = wpbp-apznr.
              COLLECT ls_rt INTO rt .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*      ENDIF.

      READ TABLE rt WITH KEY lgart = r_lga-low
                           apznr = wpbp-apznr.
      IF sy-subrc = 0 .
*        IF /dsl/hr20_t053-fdamg IS NOT INITIAL.
        CLEAR ls_rt .
        READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
        gv_dmg = ls_rt-betrg = rt-betrg * lv_dmg_rate.

        READ TABLE lt_df WITH KEY lgart = '/DMM'
                                  apznr = wpbp-apznr.
        IF sy-subrc EQ 0.
          lv_kdmgm = lt_df-betrg - lv_dmuaf.
          IF lv_dmuaf LT lt_df-betrg.
            IF gv_dmg LT lv_kdmgm.
              lv_dmuaf = lv_dmuaf + gv_dmg.
              gv_dmg = '0'.
            ELSE.
              gv_dmg = gv_dmg - lv_kdmgm.
              "lv_kdmgm = lt_df-betrg - lv_dmuaf.
              lt_df-betrg = lt_df-betrg - lv_kdmgm.
              lv_dmuaf = lv_dmuaf + lv_kdmgm.
            ENDIF.
          ENDIF.
          MODIFY lt_df TRANSPORTING betrg
                  WHERE lgart EQ lt_df-lgart
                    AND apznr EQ wpbp-apznr  .
        ENDIF.

        CLEAR ls_rt .
        READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
        IF /dsl/hr20_t053-fdamg IS NOT INITIAL.
          ls_rt-lgart = /dsl/hr20_t053-fdamg .
          ls_rt-betrg = gv_dmg .

          CLEAR: ls_rt-betpe,ls_rt-anzhl.
          ls_rt-apznr = wpbp-apznr.
          COLLECT ls_rt INTO rt .
        ENDIF.
      ENDIF.

      READ TABLE rt WITH KEY lgart = r_lga-low
                           apznr = wpbp-apznr .
      IF sy-subrc = 0.
        CLEAR lv_gv .
*        IF /dsl/hr20_t053-fgelr IS NOT INITIAL.
        gv_mat = rt-betrg - gv_kes .
*          CLEAR lv_gv .
        CALL FUNCTION 'HR_TR_CALC_TAX'
          EXPORTING
            acttax    = gv_mat
            kumtax    = kumule_gv
          IMPORTING
*           TAXKUS    =
            taxbtr    = lv_gv
*           PRZNT     =
          TABLES
            inttax    = lt_dilim
          EXCEPTIONS
            rc_errors = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        IF /dsl/hr20_t053-fgelr IS NOT INITIAL.
          CLEAR ls_rt.
          READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
          READ TABLE lt_df WITH KEY lgart = '/FRB'
                                    apznr = wpbp-apznr.
          IF sy-subrc EQ 0.
            lv_kglrm = lt_df-betrg - lv_gmuaf.
            IF lv_gmuaf LT lt_df-betrg.
              IF lv_gv LT lv_kglrm.
                lv_gmuaf = lv_gmuaf + lv_gv.
                lv_gv = '0'.
              ELSE.
                lv_gv = lv_gv - lv_kglrm.
                "lv_kglrm = lt_df-betrg - lv_gmuaf.
                lt_df-betrg = lt_df-betrg - lv_kglrm.
                lv_gmuaf = lv_gmuaf + lv_kglrm.
              ENDIF.
            ENDIF.
            MODIFY lt_df TRANSPORTING betrg
                  WHERE lgart EQ lt_df-lgart
                    AND apznr EQ wpbp-apznr  .
          ENDIF.
          CLEAR ls_rt .
          READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                         apznr = wpbp-apznr.
          IF /dsl/hr20_t053-fgelr IS NOT INITIAL.
            ls_rt-lgart = /dsl/hr20_t053-fgelr .
            ls_rt-betrg = lv_gv .

            CLEAR: ls_rt-betpe,ls_rt-anzhl.
            ls_rt-apznr = wpbp-apznr.
            COLLECT ls_rt INTO rt .
          ENDIF.

          kumule_gv = kumule_gv + gv_mat.

        ENDIF.



        IF /dsl/hr20_t053-fnett IS NOT INITIAL.
          CLEAR ls_rt .
          READ TABLE rt INTO ls_rt WITH KEY lgart = r_lga-low
                           apznr = wpbp-apznr.
          ls_rt-lgart = /dsl/hr20_t053-fnett .
          ls_rt-betrg = ls_rt-betrg - gv_kes - gv_dmg - lv_gv .
          CLEAR: ls_rt-betpe,ls_rt-anzhl.
          ls_rt-apznr = wpbp-apznr.
          COLLECT ls_rt INTO rt .
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

*  SORT rt ASCENDING BY lgart apznr .
ENDFORM.                    "brutten_nete_detay

*&---------------------------------------------------------------------*
*& Form brutten_nete_detay_brk
*&---------------------------------------------------------------------*
FORM brutten_nete_detay_brk .
  DATA lt_t052 TYPE TABLE OF /dsl/hr20_t053.
  DATA ls_t052 TYPE /dsl/hr20_t053.
  DATA : ls_rt_net LIKE rt .
  DATA : ls_rt_brt LIKE rt .
  DATA : ls_rt LIKE rt .
  DATA : lt_df LIKE TABLE OF rt WITH HEADER LINE .
  "DATA: lt_dilim TYPE TABLE OF t7trt01 WITH HEADER LINE.

  DATA : lv_ssk_hes               TYPE maxbt.
  DATA : lv_iss_hes               TYPE maxbt.
  DATA : lv_glr_hes               TYPE maxbt.
  DATA : lv_dmg_hes               TYPE maxbt.
  DATA : lv_ssk_matrah_hes        TYPE maxbt.
  DATA : lv_ssk_gun_hes           TYPE maxbt.
  DATA : lv_ssk_rate              TYPE ptr_senee.
  DATA : lv_iss_rate              TYPE ptr_senee.
  DATA : lv_devreden              TYPE maxbt.
  DATA : kumule_gv                TYPE maxbt.
  DATA : lv_dmg_rate              TYPE ptr_stamp.
  DATA : lv_ssk_tavan_gun         TYPE maxbt.
  DATA : lv_ssk_tavan             TYPE maxbt.
  DATA : kumule_ssk               TYPE maxbt.
  DATA : lv_fark                  TYPE maxbt.
  DATA : lv_kdmgm                 TYPE maxbt.
  DATA : lv_dmuaf                 TYPE maxbt.
  DATA : gv_mat                   TYPE maxbt.
  DATA : lv_gmuaf                 TYPE maxbt.
  DATA : lv_kglrm                 TYPE maxbt.

  DATA : BEGIN OF ls_vals ,
            kes                   TYPE maxbt,
            dmg                   TYPE maxbt,
            gv                    TYPE maxbt,
         END OF ls_vals .

  DEFINE read_brut.
    clear : &3.
    if &2 is not initial .
      read table rt into &3
                with key lgart = &1
                         apznr = &2 .
    else.
      read table rt into &3
                with key lgart = &1  .
    endif.

  END-OF-DEFINITION.

  DEFINE read_rt_constants.
    clear : &3.
    if &2 is not initial .
      read table rt with key lgart = &1
                             apznr = &2 .
      if sy-subrc = 0 .
        &3 = rt-betrg .
      endif.
    else.
      read table rt with key lgart = &1 .
      if sy-subrc = 0 .
        &3 = rt-betrg .
      endif.
    endif.

  END-OF-DEFINITION.

  DEFINE collect_rt.
    ls_rt-lgart = &1 .
    ls_rt-betrg = &2 .
    clear: ls_rt-betpe,ls_rt-anzhl.
    ls_rt-apznr = &3.
    collect ls_rt into rt . clear ls_rt.
  END-OF-DEFINITION.


  SELECT * FROM /dsl/hr20_t053 INTO TABLE lt_t052.

  SORT lt_t052 ASCENDING BY fbrut.


  REFRESH lt_df.
  LOOP AT wpbp.
    LOOP AT rt WHERE lgart EQ '/DMM' OR lgart EQ '/FRB'.
      MOVE-CORRESPONDING rt TO lt_df.
      lt_df-betrg = ( ( lt_df-betrg / 30 ) * wpbp-ksoll ).
      lt_df-apznr = wpbp-apznr.
      COLLECT lt_df.
    ENDLOOP.
  ENDLOOP.


  LOOP AT wpbp .
    CLEAR :lv_gmuaf, lv_dmuaf, lv_kdmgm, lv_kglrm.
    read_rt_constants : '/NEE' wpbp-apznr lv_ssk_hes        ,
                        '/IEE' wpbp-apznr lv_iss_hes        ,
                        '/SAN' wpbp-apznr lv_glr_hes        ,
                        '/S01' wpbp-apznr lv_dmg_hes        ,
                        '/104' wpbp-apznr lv_ssk_matrah_hes ,
                        '/DMT' space      lv_devreden        .
    READ TABLE rt WITH KEY lgart = '/NDY'
                           apznr = wpbp-apznr.
    IF sy-subrc = 0 .
      lv_ssk_gun_hes = rt-anzhl .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/PNE'
                           apznr = wpbp-apznr.
    IF sy-subrc = 0 .
      lv_ssk_rate = rt-anzhl / 100 .
    ENDIF.
    READ TABLE rt WITH KEY lgart = '/PIE'
                           apznr = wpbp-apznr.
    IF sy-subrc = 0 .
      lv_iss_rate = rt-anzhl / 100  .
    ENDIF.

    READ TABLE crt WITH KEY lgart = '/102' .
    IF sy-subrc = 0 .
      kumule_gv = crt-betrg.
    ELSE.
      CLEAR kumule_gv.
    ENDIF.

    PERFORM re7trg04 USING wpbp-werks wpbp-btrtl.
    PERFORM re7trt01 USING t7trg04-grtax wpbp-begda wpbp-endda.
    PERFORM re7trp02 USING t7trg04-grstx wpbp-begda  wpbp-endda.

    lv_dmg_rate = t7trp02-stamp.
    lv_ssk_tavan_gun = t7trs02-sskmx / 30 .
    lv_ssk_tavan = ( t7trs02-sskmx / 30 ) * lv_ssk_gun_hes .

    LOOP AT lt_t052 INTO ls_t052.

      CLEAR ls_rt_brt.
      CLEAR ls_vals.
      READ TABLE rt INTO ls_rt_brt
               WITH KEY lgart = ls_t052-fbrut
                        apznr = wpbp-apznr.

      CLEAR ls_rt_net.
      READ TABLE rt INTO ls_rt_net
               WITH KEY lgart = ls_t052-fnett
                        apznr = wpbp-apznr.
      CHECK sy-subrc NE 0 AND ls_rt_brt-betrg IS NOT INITIAL.

      CLEAR: ls_vals .
**********************************************************************
***      <<----   SSK İşçi Payı  ---->>
      IF ls_t052-fssk IS NOT INITIAL.
        IF kumule_ssk < lv_ssk_matrah_hes.
          kumule_ssk = kumule_ssk + ls_rt_brt-betrg .
          IF kumule_ssk >= lv_ssk_matrah_hes .
            lv_fark = kumule_ssk - ls_rt_brt-betrg .
            IF lv_fark < 0 .
              lv_fark = 0 .
            ENDIF.
            read_brut : ls_t052-fbrut wpbp-apznr ls_rt.
            ls_rt-betrg = ( lv_ssk_matrah_hes - lv_fark ) * lv_ssk_rate.
            ls_vals-kes = ls_vals-kes + ls_rt-betrg .
            collect_rt : ls_t052-fssk ls_rt-betrg wpbp-apznr.

***         <<----   İşsizlik Payı  ---->>
            IF ls_t052-fissz IS NOT INITIAL.
              read_brut : ls_t052-fbrut wpbp-apznr ls_rt.
              lv_fark = kumule_ssk - ls_rt-betrg .
              IF lv_fark < 0 .
                lv_fark = 0 .
              ENDIF.
              ls_rt-betrg = ( lv_ssk_matrah_hes - lv_fark ) *
                                    lv_iss_rate.
              ls_vals-kes = ls_vals-kes + ls_rt-betrg .
              collect_rt : ls_t052-fissz ls_rt-betrg wpbp-apznr.
            ENDIF.
***         <<----   İşsizlik Payı  ---->>

          ELSE.
            read_brut : ls_t052-fbrut wpbp-apznr ls_rt.
            ls_rt-betrg = ( ls_rt-betrg + lv_devreden ) * lv_ssk_rate .
            ls_vals-kes = ls_vals-kes + ls_rt-betrg .
            collect_rt : ls_t052-fssk ls_rt-betrg wpbp-apznr.
***         <<----   İşsizlik Payı  ---->>
            IF ls_t052-fissz IS NOT INITIAL.
              read_brut : ls_t052-fbrut wpbp-apznr ls_rt.
              ls_rt-betrg = ( rt-betrg + lv_devreden ) * lv_iss_rate .
              lv_devreden = 0.
              ls_vals-kes = ls_vals-kes + ls_rt-betrg .
              collect_rt : ls_t052-fissz ls_rt-betrg wpbp-apznr.
            ENDIF.
***         <<----   İşsizlik Payı  ---->>
          ENDIF.
        ENDIF.
      ENDIF.
**********************************************************************
***   <<----   Damga Vergisi  ---->>
      IF ls_t052-fdamg IS NOT INITIAL .
        read_brut : ls_t052-fbrut wpbp-apznr ls_rt_brt.
        ls_vals-dmg = ls_rt_brt-betrg * lv_dmg_rate.
        CLEAR : lt_df.
        READ TABLE lt_df WITH KEY lgart = '/DMM'
                                  apznr = wpbp-apznr.
        IF sy-subrc EQ 0.
          lv_kdmgm = lt_df-betrg - lv_dmuaf.
          IF lv_dmuaf LT lt_df-betrg.
            IF ls_vals-dmg LT lv_kdmgm.
              lv_dmuaf = lv_dmuaf + ls_vals-dmg.
              ls_vals-dmg = '0'.
            ELSE.
              ls_vals-dmg = ls_vals-dmg - lv_kdmgm.
              "lv_kdmgm = lt_df-betrg - lv_dmuaf.
              lt_df-betrg = lt_df-betrg - lv_kdmgm.
              lv_dmuaf = lv_dmuaf + lv_kdmgm.
            ENDIF.
          ENDIF.
          MODIFY lt_df TRANSPORTING betrg
                    WHERE lgart EQ lt_df-lgart
                      AND apznr EQ wpbp-apznr  .
        ENDIF.
        read_brut : ls_t052-fbrut wpbp-apznr ls_rt.
        collect_rt : ls_t052-fdamg ls_vals-dmg wpbp-apznr.
      ENDIF.
***   <<----   Damga Vergisi  ---->>
**********************************************************************
***   <<----   Gelir Vergisi  ---->>
      IF ls_t052-fgelr IS NOT INITIAL .
        read_brut : ls_t052-fbrut wpbp-apznr ls_rt.
        gv_mat = ls_rt-betrg - ls_vals-kes .

        CALL FUNCTION 'HR_TR_CALC_TAX'
          EXPORTING
            acttax    = gv_mat
            kumtax    = kumule_gv
          IMPORTING
            taxbtr    = ls_vals-gv
          TABLES
            inttax    = inttax[]
          EXCEPTIONS
            rc_errors = 1
            OTHERS    = 2.

        READ TABLE lt_df WITH KEY lgart = '/FRB'
                                  apznr = wpbp-apznr .
        IF sy-subrc EQ 0.
          lv_kglrm = lt_df-betrg - lv_gmuaf.
          IF lv_gmuaf LT lt_df-betrg.
            IF ls_vals-gv LT lv_kglrm.
              lv_gmuaf = lv_gmuaf + ls_vals-gv.
              ls_vals-gv = '0'.
            ELSE.
              ls_vals-gv = ls_vals-gv - lv_kglrm.
              "lv_kglrm = lt_df-betrg - lv_gmuaf.
              lt_df-betrg = lt_df-betrg - lv_kglrm.
              lv_gmuaf = lv_gmuaf + lv_kglrm.
            ENDIF.
          ENDIF.

          MODIFY lt_df TRANSPORTING betrg
                    WHERE lgart EQ lt_df-lgart
                      AND apznr EQ wpbp-apznr  .
        ENDIF.
        read_brut : ls_t052-fbrut wpbp-apznr ls_rt.
        collect_rt : ls_t052-fgelr ls_vals-gv wpbp-apznr.
        kumule_gv = kumule_gv + gv_mat.
      ENDIF.
***   <<----   Gelir Vergisi  ---->>
**********************************************************************
***   <<----   Net Ücret Türü  ---->>
      IF ls_t052-fnett IS NOT INITIAL.
        read_brut : ls_t052-fbrut wpbp-apznr ls_rt.
        ls_rt-betrg = ls_rt-betrg - ls_vals-kes -
                      ls_vals-dmg - ls_vals-gv .
        collect_rt : ls_t052-fnett ls_rt-betrg wpbp-apznr.
      ENDIF.
***   <<----   Net Ücret Türü  ---->>
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    "brutten_nete_detay_brk
