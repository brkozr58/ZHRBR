*&---------------------------------------------------------------------*
*& Include          ZHRBR_P004_I001
*&---------------------------------------------------------------------*
TABLES : sscrfields , pa0015 .
INCLUDE : icons ."zhriga_p037_i006. " text include

CLASS lcl_report DEFINITION DEFERRED.

DATA  gr_report TYPE REF TO lcl_report.

"function key
DATA : sc_fc01  TYPE smp_dyntxt.

" main datas


" dosya adı
DATA : gv_sample_file LIKE rlgrap-filename.


" oluşturulacak örnek dosyanın head kısmı
DATA : BEGIN OF gs_head,
         header(30),                     " Header Data
       END OF gs_head,
       gt_head LIKE TABLE OF gs_head.

" oluşturulacak örnek dosyanın veri kısmı


DATA: lt_p0000 TYPE TABLE OF pa0000,
      ls_p0000 TYPE pa0000.

DATA :lv_begda TYPE begda.



    DATA : BEGIN OF gs_sabit,
              pernr TYPE persno ,
              subty TYPE subty ,
              begda TYPE begda ,
              endda TYPE endda ,
              dossr TYPE zhrbr_de023 ,
              dosno TYPE zhrbr_de019 ,
              kessr TYPE zhrbr_de001 ,
              icrad TYPE zhrbr_de020 ,
              alici TYPE zhrbr_de024 ,
              aiban TYPE zhrbr_de025 ,
              hspsk TYPE zhrbr_de005 ,
              tkstt TYPE zhrbr_de014 ,
              waers TYPE waers ,
              acilm TYPE zhrbr_de034 ,
              icon  TYPE  icon_d,
              messa	TYPE 	msgtx,
              mark,
           END OF gs_sabit,
           gt_sabit LIKE TABLE OF gs_sabit  .
   DATA : BEGIN OF gs_yuzde,
              pernr TYPE persno ,
              subty TYPE subty  ,
              begda TYPE begda  ,
              endda TYPE endda  ,
              dossr TYPE zhrbr_de023 ,
              dosno TYPE zhrbr_de019 ,
              kessr TYPE zhrbr_de001 ,
              icrad TYPE zhrbr_de020 ,
              alici TYPE zhrbr_de024 ,
              aiban TYPE zhrbr_de025 ,
              hspsk TYPE zhrbr_de014 ,
              icrtr TYPE zhrbr_de012 ,
              waers TYPE waers ,
              faizt TYPE zhrbr_de017 ,
              orng1 TYPE zhrbr_de006 ,
              orny1 TYPE zhrbr_de015 ,
              orng2 TYPE zhrbr_de006 ,
              orny2 TYPE zhrbr_de015 ,
*              orng3 TYPE zhrbr_de006 ,
*              orny3 TYPE zhrbr_de015 ,
*              orng4 TYPE zhrbr_de006 ,
*              orny4 TYPE zhrbr_de015 ,
              acilm TYPE zhrbr_de034 ,
              icon  TYPE  icon_d,
              messa	TYPE 	msgtx,
              mark,
           END OF gs_yuzde,
           gt_yuzde LIKE TABLE OF gs_yuzde .
    DATA : BEGIN OF gs_kesnt,
              pernr TYPE persno ,
              subty TYPE subty  ,
              begda TYPE begda  ,
              endda TYPE endda  ,
              icrid TYPE zhrbr_de030,
              odmtr TYPE zhrbr_de022,
              manue TYPE zhrbr_de026 ,
              icon  TYPE  icon_d,
              messa	TYPE 	msgtx,
              mark,
           END OF gs_kesnt,
           gt_kesnt LIKE TABLE OF gs_kesnt .


    DATA : BEGIN OF ls_sabit,
              pernr(8),
              subty(4),
              begda(10),
              endda(10),
              dossr(20),
              dosno(20),
              kessr(3),
              icrad(4),
              alici(40),
              aiban(34),
              hspsk(2),
              tkstt(12),
              waers(5),
              acilm(255),
           END OF ls_sabit,
           lt_sabit LIKE TABLE OF ls_sabit.
   DATA : BEGIN OF ls_yuzde,
              pernr(8),
              subty(4),
              begda(10),
              endda(10),
              dossr(20),
              dosno(20),
              kessr(3),
              icrad(4),
              alici(40),
              aiban(34),
              hspsk(2),
              icrtr(12),
              waers(5),
              faizt(12),
              orng1(2),
              orny1(3),
              orng2(2),
              orny2(3),
*              orng3(3),
*              orny3(3),
*              orng4(3),
*              orny4(3),
              acilm(255),
           END OF ls_yuzde,
           lt_yuzde LIKE TABLE OF ls_yuzde.
    DATA : BEGIN OF ls_kesnt,
              pernr(8),
              subty(4),
              begda(10),
              endda(10),
              icrid(10),
              odmtr(12),
              manue(1),
           END OF ls_kesnt,
           lt_kesnt LIKE TABLE OF ls_kesnt.
