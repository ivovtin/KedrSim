#ifndef __MU_SIM_H__
#define __MU_SIM_H__

#include<KrMu/mu_system.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void mu_get_pdata_(double *, short *, short *);

struct {
	short mu_hits, mu_n_hits;
	short mu_ch_hit[MU_NUMBER_OF_CHANNELS];
	short mu_ch_time[MU_NUMBER_OF_CHANNELS];
	short first_crate;
} mu_hit_data_;

extern struct {
	float p[4];
} mu_par_;
#define mu_resolution mu_par_.p[0]
#define mu_efficiency mu_par_.p[1]
#define mu_debug mu_par_.p[2]
#define mu_write mu_par_.p[3]

#define mu_n_hits mu_hit_data_.mu_n_hits
#define mu_ch_hit(i) mu_hit_data_.mu_ch_hit[i]
#define mu_ch_time(i) mu_hit_data_.mu_ch_time[i]

void mu_default_init_(void);
void muon_hit_find_(void);

#ifdef __cplusplus
}
#endif

#endif /* __MU_SIM_H__ */
