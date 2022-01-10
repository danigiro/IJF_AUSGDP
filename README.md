
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IJF_AUSGDP

<!-- badges: start -->
<!-- badges: end -->

Reconciliation of quarterly Australian GDP forecasts from Income and
Expenditure sides

## Files

-   **BaseForecasts**: scripts to use the base forecasts in
    Bisaglia (2020) and to calculate the naive forecasts:
    -   `TYPE/TYPE_features_snaive.R`: Seasonal Random Walk forecasts
        and ARIMA base forecasts dataset;
    -   `TYPE/TYPE_features_sa.R`: Seasonal Average forecasts.
-   **Reconciliation**:
    -   `TYPE/ARIMA`:
        -   `TYPE_cslccd_bCCCred.R`: LCC with ARIMA bts
        -   `TYPE_cslccd_bLCC.R`: LxCC with ARIMA bts (x = number of
            level)
        -   `TYPE_cslccd_mean_red.R`: LCC with SRW bts
        -   `TYPE_cslccd_mean.R`: CCC with SRW bts
        -   `TYPE_cslccd_mLCC.R`: LxCC with SRW bts (x = number of
            level)
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

Bisaglia, L., Di Fonzo, T., Girolimetto, D., 2020. *Fully reconciled GDP
forecasts from Income and Expenditure sides*, in: Pollice, A., Salvati,
N., Schirripa Spagnolo, F. (Eds.), Book of Short Papers SIS 2020. Pear-
son, pp. 951–956. URL:
<https://it.pearson.com/content/dam/region-core/italy/pearson-italy/pdf/Docenti/Universit%C3%A0/Pearson-SIS-2020-atti-convegno.pdf>.
