#include<iostream>

#include<KcMagField/KcMagField.hh>

extern "C" {
extern void gufld_simple_(float *, float*);

static void (*gufld)(float *, float *) = 0;
static KcMagField *M = 0;

extern struct {
	float fiel, fiean, fieldmap;
} fiel_;

static float hall_pos[3] = { 0., 55.5, 56.0 };
static float convert = 1.E-3;

#define vect(x) "(" << x[0] << ", " << x[1] << ", " << x[2] << ")"

void gufld_complex(float *v, float *f)
{
	double bx, by, bz;
	M->Get(v[0], v[1], v[2], bx, by, bz);
	f[0] = bx * convert;
	f[1] = by * convert;
	f[2] = bz * convert;

//	std::cout << vect(v) << ": " << vect(f) << std::endl;
}

void set_gufld_simple_(void)
{
	std::cout << "Simple GUFLD routine will be used for magnetic "
	          << "field calculation" << std::endl;
	gufld = gufld_simple_;
}

void set_gufld_complex_(void)
{
	std::cout << "Complex GUFLD routine will be used for magnetic "
	          << "field calculation" << std::endl;
	gufld = gufld_complex;
	M = new KcMagField(".");
	if(M == 0){
		std::cerr << "Cannot initialize magfield map from current directory,"
		          << " make sure you have mag.mag and mg3.mg3 there"
				  << std::endl;
		exit(1);
	}
	
	float f[3];
	float &mapval = f[2];
	gufld_complex(hall_pos, f);
	
	convert *= fiel_.fiel / mapval;
	
	std::cout << "Set mag. field is " << fiel_.fiel << std::endl
	          << "Map field at " << vect(hall_pos) << " is " << mapval
			  << std::endl << "Conversion coefficient is " << convert * 1000.
			  << std::endl;
}

void gufld_(float *vect, float *field)
{
	(*gufld)(vect, field);
}

};
