# SCELBAL BASIC for 8008 loader on DOS

8008CPU用で実数が扱えるSCELBAL BASIC を  
DOS上のエミュレータで動作させる試みです。  

## ビルド環境

OS:Windows 11 コマンドプロンプト  
アセンブラ：Macro assembler  AS V1.42 Beta [Bld 271]  

## 実行環境

OS:MS-DOS 2.11以降 必要メモリ192KB

ファイル構成  
SC1.ASM ----- ソースファイル（8080用と共通ソース）  
SC1.BAT ----- アセンブル～実行ファイル（拡張子 .808）を一連で作成するバッチ  
SC1.LST ----- ASが生成するリスティングファイル  
SC1.808 ----- 8008ローダー用実行形式ファイル  
SC1_CPM.COM - 8080用にアセンブルしたCP/M用実行形式ファイル  
8008EM.BIN -- 8008エミュレーター  
8008.COM ---- DOS用8008ローダー  
ASCII08.BAS - ASCIIARTをSCELBAL BASICに合わせて修正したもの(Ryo Mukaiさん作)  
SC1FAST.ASM - 公開されている旧ニモニックのSELBEI用ソース  

## 使い方

MS-DOS環境にて、カレントディレクトリまたは  
環境変数 Z_EMで示されるディレクトリに  
8008EM.BINおよびSC1.808を置き  

8008 sc1にて実行します  

LOADおよびSAVE機能は未実装です、実行するとDOSに戻ります。  
(DOSに戻る SYSTEMコマンドの代わりに使ってください）  

## ASCIIART実行速度について

クロック12.8MHzのV20で動作するMS-DOS上の  
8008 loader for DOS にて所要時間 2時間33分でした。  
Ryo Mukaiさんの500kHz動作の実機がオリジナルのSCELBALで  
6時間40分 Faster SSELBALだと10%高速で6時間00分とすると  
実機1.2MHz動作程度の速度のようです。  

## 8008と8080の命令の互換性について

新ニモニック表記した場合、ハードやOS依存部分以外は、  
8080は8008に対してアセンブラレベルでは完全上位互換となっています。  
本トライでは、オリジナルのソースの旧ニモニックを新ニモニックに  
置き換えて8080用にもそのまま使えることを確かめてみました。  

ただし、コンソール入出力をCP/Mで実現しようとすると  
ファンクションコールでレジスタが破壊されてしまうため  
8080専用コード部分ではPUSH/POP等拡張された命令を使いました。  

## 8080での実行につて

ソース87行目を 
FOR8080	EQU	1  
としてアセンブルするとCP/M上の8080で動作させることができます。  

## ライセンス

私が改変した部分につきましてはMITライセンスとします。  
元のSCELBALに関しては非商用に限り利用可のようです  
詳細は参照元サイトをご覧ください。  

## 8008エミュレータ

8008機械語を解釈実行する部分は別モジュールとして分離しています。  
詳細は下記リポジトリをご参照ください。  
https://github.com/Gazelle8087/8008-instruction-executor-for-x86

## 8008 loader for DOS について

8008用バイナリをロードし上記8008エミュレータを使って  
実行するローダーです  
詳細は下記リポジトリそご参照ください。  
https://github.com/Gazelle8087/8008-loader-for-DOS

## 8008 CPU について

GNDが無い、リセット端子が無い、等、18ピンに収めるため  
工夫（苦労）しているようです。  

データシートは以下リンクを参照ください。  
http://www.bitsavers.org/components/intel/MCS8/Intel_8008_8-Bit_Parallel_Central_Processing_Unit_Rev1_Apr72.pdf

8008ファンさんのサイトです。  
http://www.mars.dti.ne.jp/~mark08/index.html

半導体コレクション展示会場  
他のCPUと共に詳細に解説されています。  
http://www.st.rim.or.jp/~nkomatsu/intel8bit/i8008.html

## 8008 CPU 搭載ミニコン SCELBIについて

詳細な資料が集約されています。  
https://www.scelbi.com/

実数型 SCELBAL BASIC はソース含め公開されています。  
https://www.willegal.net/scelbi/the8008andScelbi.html

## 8008実機動作事例

Ryo MukaiさんがTangNanoで8008を動作させています。  
ASCIIARTのBASICソースもこちらからいただきました。  
https://github.com/ryomuk/tangnano-5V/tree/main/applications/TangNano8008MEM

私もEMUZ80と同様の手法で2チップ構成で8008を動作させてみたいと思っているのですが  
チップ単体が送料含めると10,000円程度と高価なのでどうしたものか思案中です。

## 変更履歴
2025/6/5 Rev. 1.00	初回公開

