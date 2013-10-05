#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <float.h>
#include <math.h>

#define LETTERS 25

typedef struct histogram
{
    double data[LETTERS];
} histogram;

typedef struct text_file
{
    FILE* f;
    int32_t length;
} text_file;

text_file* load_file(char* file_path)
{
    FILE* f = fopen(file_path, "r");
    text_file* tf = (text_file*)malloc(sizeof(text_file));
    
    if (f == NULL)
    {
	return NULL;
    }

    fseek(f, 0, SEEK_END);
    int32_t size = ftell(f);
    rewind(f);
    
    tf->length = size;
    tf->f = f;

    return tf;
}

histogram* make_histogram(char* file_path)
{
    text_file *f = load_file(file_path);
    int data[LETTERS] = {0};
    histogram *h = (histogram*)malloc(sizeof(histogram));
    char c;
    
    if (f == NULL)
    {
	return NULL;
    }

    //make histogram
    while (feof(f->f) == 0)
    {
	c = fgetc(f->f);

	if (isalpha(c))
	{
	    data[toupper(c) - 'A']++;
	}
    }

    for (int i=0; i<LETTERS; i++)
    {
	h->data[i] = (double)(data[i]/(double)f->length);
    }
    
    return h;
}

int main(int argc, char** argv)
{
    double lowest = DBL_MAX;
    int lowest_i = 0;
    double tmp = 0.0;
    
    if (argc < 2)
    {
        printf("usage:...");
        return 1;
    }
    
    histogram *dict = make_histogram(argv[1]);
    histogram *cipher_text = make_histogram(argv[2]);

    for (int i=0;i<LETTERS;i++)
    {
	tmp = 0.0;

	for (int j=0;j<LETTERS;j++)
	{
	    double a = cipher_text->data[(i+j)%LETTERS] - dict->data[j];

	    if (a < 0)
		a = -a;

	    tmp += a;
	}
       
	//printf("%d\t%f\n", i, tmp);

	if (tmp < lowest)
	{
	    lowest = tmp;
	    lowest_i = i;
	}
    }

    printf("%d\n", lowest_i > 14 ? lowest_i+1 : lowest_i);
    
    return 0;
}
