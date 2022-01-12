
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IJF_AUSGDP

<!-- badges: start -->
<!-- badges: end -->

Forecast combination based reconciliation of the quarterly Australian
GDP from Income and Expenditure sides (Di Fonzo and Girolimetto, 2022)

The forecast reconciliation is performed using **FoReco 0.2.2**.

## Files

**Note:** the analysis folders are distinct according to two different
hierarchies. *TYPE* stands for either Income/INC or Expenditure/EXP.

-   **BaseForecasts**: Base forecasts as in Athanasopoulos et al. (2019)
    and naive forecasts (SA and SRW):
    -   `TYPE/TYPE_features_snaive.R`: Seasonal Random Walk (SRW) and
        ARIMA base forecasts;
    -   `TYPE/TYPE_features_sa.R`: Seasonal Average (SA) base forecasts.
-   **Reconciliation**:
    -   `TYPE/ARIMA`:
        -   `TYPE_cslccd_bCCCred.R`: LCC with ARIMA bts
        -   `TYPE_cslccd_bLCC.R`: LxCC with ARIMA bts (x = level number)
        -   `TYPE_cslccd_mean_red.R`: LCC with SRW bts
        -   `TYPE_cslccd_mean.R`: CCC with SRW bts
        -   `TYPE_cslccd_mLCC.R`: LxCC with SRW bts (x = level number)
        -   `TYPE_cslccd_sa.R`: CCC with SA bts
        -   `TYPE_cslccd.R`: CCC with ARIMA bts and CCCH
        -   `TYPE_htsrec_mean.R`: cross-sectional reconciliation with
            SRW bts
        -   `TYPE_htsrec.R`: cross-sectional reconciliation with ARIMA
            bts
        -   `TYPE_CCCmix_red.R`: sample average of LCC (SRW + ARIMA bts)
        -   `TYPE_CCCmix.R`: sample average of CCC (SRW + ARIMA bts)
        -   `TYPE_csmix.R`: sample average of the cross-sectional
            reconciliation (SRW + ARIMA bts)
    -   `TYPE/function`: functions for `TYPE/score/TYPE_scores_arima.R`
    -   `TYPE/score/TYPE_scores_arima.R`: returns the scores’ dataset

## References

Athanasopoulos, G., Gamakumara, P., Panagiotelis, A., Hyndman, R.J.,
Affan, M., 2019. *Hierarchical Forecasting*, in: Fuleky, P. (Ed.),
Macroeconomic Forecasting in the Era of Big Data. Springer, Cham,
pp. 689–719. <doi:10.1007/978-3-030-31150-6_21>.

Di Fonzo, T. and Girolimetto D. (2022). *Forecast combination based
forecast reconciliation: insights and extensions*, preprint submitted to
the International Journal of Forecasting
