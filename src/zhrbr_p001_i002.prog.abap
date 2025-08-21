*&---------------------------------------------------------------------*
*& Include          zhrbr_P001_I002
*&---------------------------------------------------------------------*
PARAMETERS: p_pernr TYPE persno OBLIGATORY MATCHCODE OBJECT prem,
            p_dosno TYPE zhrbr_de019 AS LISTBOX VISIBLE LENGTH 20. "TYPE ZHRPA_DE016 .

" Sıraya Girecek Kesinti Cevap Yazısı
SELECTION-SCREEN BEGIN OF SCREEN 110 AS SUBSCREEN NESTING LEVEL 4.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s10.
PARAMETERS: p1_icrad TYPE zhrbr_de029 , "icra adı
*            p1_icryr TYPE char200 , " , "icra yeri
*            p1_icry2 TYPE char200,
*            p1_usrid TYPE sysid,
            p1_evtrh TYPE datum       , "evrak tarihi
            p1_dosno TYPE zhrbr_de019 , "dosya no
*            p1_yonad TYPE emnam       , "yönetici adı
*            p1_yonun TYPE stext       . "yönetici ünvanı
            p1_imza1 TYPE persno MATCHCODE OBJECT prem.
*            p1_imza2 TYPE persno MATCHCODE OBJECT prem.

SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 110.

" Çalışan Personel için Kesinti Yapılacağına Dair Cevap Yazısı
SELECTION-SCREEN BEGIN OF SCREEN 120 AS SUBSCREEN NESTING LEVEL 4.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s20.
PARAMETERS: p2_kytno TYPE zhrbr_de033 , "sayı
*            p2_yonad TYPE emnam       , "yönetici adı
*            p2_yonun TYPE stext       . "yönetici ünvanı
            p2_evtrh TYPE datum,
            p2_imza1 TYPE persno MATCHCODE OBJECT prem.
*            p2_imza2 TYPE persno MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF SCREEN 120.

" Çıkış Bilgi Yazısı
SELECTION-SCREEN BEGIN OF SCREEN 130 AS SUBSCREEN NESTING LEVEL 4.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s30.
PARAMETERS: p3_kytno TYPE zhrbr_de033 , "sayı
*            p3_yonad TYPE emnam       , "yönetici adı
*            p3_yonun TYPE stext       . "yönetici ünvanı
            p3_evtrh TYPE datum        , "evrak tarihi
            p3_imza1 TYPE persno MATCHCODE OBJECT prem.
*            p3_imza2 TYPE persno MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF SCREEN 130.

" Eski İşten Ayrılmış Personel İcra Yazısı
SELECTION-SCREEN BEGIN OF SCREEN 140 AS SUBSCREEN NESTING LEVEL 4.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-s40.
PARAMETERS: "p4_kytno TYPE zhrpa_de053 , "sayı
             p4_evtrh TYPE datum       , "evrak tarihi
             p4_dosno TYPE zhrbr_de019 , "dosya no
             p4_icrad TYPE zhrbr_de029 , "icra adı
*             p4_icryr TYPE zhrbr_de028 , "icra yeri
*             p4_icry2 TYPE char200,
*             p4_usrid TYPE sysid.
             p4_imza1 TYPE persno MATCHCODE OBJECT prem.
*             p4_imza2 TYPE persno MATCHCODE OBJECT prem.
*            p4_yonad TYPE emnam       , "yönetici adı
*            p4_yonun TYPE stext       . "yönetici ünvanı
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN END OF SCREEN 140.
