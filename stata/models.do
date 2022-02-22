* set panel identifiers
xtset firm_id ano

********* FULL *******
xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i(2013 2014 2015 2016 2017 2018 2019).ano, /*
*/ gmmstyle(L.(inv), lag (2 3)) /*
*/ gmmstyle(fc, lag (2 4)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i(2013 2014 2015 2016 2017 2018 2019).ano, /*
*/ gmmstyle(L.(inv), lag (2 3)) /*
*/ gmmstyle(fc, lag (2 4)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie  i.sect i(2013 2014 2015 2016 2017 2018 2019).ano, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 4)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie i.sect i(2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i(2013 2014 2015 2016 2017 2018 2019).ano, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 4)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i(2013 2014 2015 2016 2017 2018 2019).ano, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 4)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (2 2)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

********* CONSTRAINTS *******

xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano if dum_kz==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano if dum_kz==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie  i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_kz==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_kz==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_kz==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (2 2)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_kz==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_kz==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie  i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_kz==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_kz==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_kz==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (2 2)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano if dum_ww==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ iv(ln_epu i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano if dum_ww==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie  i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_ww==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 2)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i(2014 2015 2016 2017 2018 2019).ano if dum_ww==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 2)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano, eq(level)) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie  cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_ww==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 3)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (2 2)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano, eq(level)) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_ww==0, /*
*/ gmmstyle(L.(inv), lag (2 4)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_ww==0, /*
*/ gmmstyle(L.(inv), lag (2 4)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie  i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_ww==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 2)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_ww==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 2)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_ww==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 2)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (0 0)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (2 2)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 3)) /*
*/ iv(ln_epu i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==1, /*
*/ gmmstyle(L.(inv), lag (2 2) collapse) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (0 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (1 3)) /*
*/ gmmstyle(q_tobin, lag (1 1) collapse) /*
*/ iv(ln_epu i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==1, /*
*/ gmmstyle(L.(inv), lag (2 2) collapse) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (0 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (1 3)) /*
*/ gmmstyle(q_tobin, lag (1 1) collapse) /*
*/ iv(ln_epu ln_iie i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==1, /*
*/ gmmstyle(L.(inv), lag (2 2) collapse) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (0 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (1 3)) /*
*/ gmmstyle(q_tobin, lag (1 1) collapse) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==1, /*
*/ gmmstyle(L.(inv), lag (2 2) collapse) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (0 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (1 3)) /*
*/ gmmstyle(q_tobin, lag (1 1) collapse) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (2 2)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano, eq(level)) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ iv(ln_epu i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1) collapse) /*
*/ iv(ln_epu i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1) collapse) /*
*/ iv(ln_epu ln_iie i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1) collapse) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_sa==0, /*
*/ gmmstyle(L.(inv), lag (2 2) ) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 2)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1) collapse) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (2 2)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i( 2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

********* CAPITAL INTENSITY *******
xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie  i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==1, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (1 1)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==0, /*
*/ gmmstyle(L.(inv), lag (2 3)) /*
*/ gmmstyle(fc, lag (2 3)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin i.sect i( 2013 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie  i.sect i(2013 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan

xtabond2 inv L.inv ln_epu fc c.ln_epu#c.fc cv divida tamanho q_tobin ln_iie cresc_pib i.sect i( 2014 2015 2016 2017 2018 2019).ano if dum_intensidade==0, /*
*/ gmmstyle(L.(inv), lag (2 2)) /*
*/ gmmstyle(fc, lag (2 2)) /*
*/ gmmstyle(divida, lag (1 1)) /*
*/ gmmstyle(cv, lag (1 1)) /*
*/ gmmstyle(tamanho, lag (1 1)) /*
*/ gmmstyle(q_tobin, lag (1 1)) /*
*/ gmmstyle(c.ln_epu#c.fc, lag (2 2)) /*
*/ iv(ln_epu ln_iie cresc_pib i.sect i(2012 2013 2014 2015 2016 2017 2018 2019).ano) robust small twostep h(2) nodiffsargan
