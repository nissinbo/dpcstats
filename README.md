
# dpcstats

<!-- badges: start -->
<!-- badges: end -->

日本の厚生労働省から公表されている[DPC調査結果](https://www.mhlw.go.jp/stf/shingi/shingi-chuo_128164.html)にあるエクセルデータの一部を、tidy dataに変換したデータパッケージ

## Installation

You can install the development version of dpcstats from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nissinbo/dpcstats")
```

## Example

``` r
library(dpcstats)

disease_surgery_MDC01_2022 <- dpcstats::disease_surgery_MDC01_2022
```

## 対象のデータ

https://www.mhlw.go.jp/stf/shingi/shingi-chuo_128164.html から「資料」リンクにアクセスし閲覧できるもののうち、以下のテーブルを対象とした。

- （８）疾患別手術別集計_MDC
- （９）疾患別手術有無別処置1有無別集計_MDC
- （１０）疾患別手術有無別処置2有無別集計_MDC

csvファイルでは英語のファイル名で表し、対応は以下の通り。

- disease_surg: （８）疾患別手術別集計_MDC
- disease_surg_proc1: （９）疾患別手術有無別処置1有無別集計_MDC
- disease_surg_proc2: （１０）疾患別手術有無別処置2有無別集計_MDC
