#include<math.h>
#include<stdlib.h>
#include<stdio.h>

#include<KrMu/mu_system.h>
#include<KrMu/mu_geom.h>
#include"mu_sim.h"

void muon_hit_find_(void)
{
	unsigned short ch;
	double track[6];
	short octant, layer;

	mu_get_pdata_(track, &octant, &layer);

	if(mu_debug > 1) printf("MUON_HIT_FIND: %hd %hd\n", octant, layer);

/*
	if(mu_debug > 1 /*.or.idebug.eq.1){
		printf("Real vertex: ',x1,y1,z1
			print *,'Particle momenta: ',vect(4),vect(5),vect(6),' = ',vect(7)
			print *,'Octant:',OCTANT,'  Layer:',LAYer
		endif
*/
/*		call grndm(rndm,1)
		if(rndm.le.mu_efficiency) then */ 
	for(ch = mu_octant_min_channel(octant);
		ch <= mu_octant_max_channel(octant);
		ch ++)
	{
		if(mu_channel_layer(ch) != layer) continue;
		unsigned short ntubes = mu_channel_number_of_tubes(ch), t;
		for(t = 0; t < ntubes; t ++){
			double x = mu_channel_tube_x(ch, t);
			double y = mu_channel_tube_y(ch, t);
			double Rmin = mu_distance_to_stright(x, y, 0,
				track[0], track[1], 0, track[3], track[4], 0);
			if(Rmin - MU_TUBE_RADIUS * 0.97 < 0){
				double z = mu_tube_track_cross_z(x, y, track);
				double tube_len = mu_channel_tubes_length(ch);
				double half_tube_len = tube_len / 2;
				if(fabs(z) <= half_tube_len){
/*
проверим, нет ли уже в этом канале событий
если есть, надо что-то делать, но пока просто игнорируем все последующие
*/
					unsigned short i;
					for(i = 0; i < mu_n_hits; i ++){
						if(mu_ch_hit(i) == ch) break;
					}
					if(i == mu_n_hits){
						int c = -1;
						switch(mu_channel_number_of_tubes(ch)){
						case 4: c = 0; break;
						case 6: c = 1; break;
						}
						if(c >= 0){
							unsigned short c1 = (unsigned short)mu_round(mu_default_clbr[c][t * 2] / 10.);
							unsigned short c2 = (unsigned short)mu_round(mu_default_clbr[c][t * 2 + 1] / 10.);
							unsigned short time;
							if((t % 2) == 0)
								time = c1 + (z + half_tube_len) * (c2 - c1) / tube_len;
							else
								time = c1 + (half_tube_len - z) * (c2 - c1) / tube_len;
/*
							IF(mu_debug.ge.2.OR.IDEBUG.EQ.1)THEN
								PRINT *,'Block:',BLOCK,'  Channel:',CHANNEL,'  Tube:',TUBE
								PRINT *,'Distance to wire:',Rmin
								print *,'Xhit',Xper,'  Yhit',Yper,'  Zhit',Zper
								PRINT *,'CHANabs',CHANabs,'  Texp',Texp,' Zexp',Zexp
							ENDIF
*/
							mu_ch_hit(mu_n_hits) = ch;
							mu_ch_time(mu_n_hits) = time;
							mu_n_hits ++;
 							if(mu_debug >0){
								printf("MUON_HIT_FIND: %d %d\n", ch, time);
							}
						} /* else print error message? */
					}
				}
			}
		}
	}
/*
		elseif(mu_debug.ge.2.OR.IDEBUG.EQ.1) then
			print *,'Rejected due to unefficiency'
		endif
*/
}

void mu_default_init_(void)
{
/*
	if(verbose){
		printf("Initializing mu system data structures... ");
	}
*/
	int cc = mu_init();
	if(cc){
		printf("mu_init() failed, cc = %d\n", cc);
		fprintf(stderr, "mu_init() failed, cc = %d\n", cc);
		exit(1);
	}
	cc = mu_load_geometry(0);
	if(cc){
		printf("loading mu geometry failed, cc = %d\n", cc);
		fprintf(stderr, "loading mu geometry failed, cc = %d\n", cc);
		exit(1);
	}
}
