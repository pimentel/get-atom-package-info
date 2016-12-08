def source_r(base, fname):
    return 'OMP_NUM_THREADS=1 Rscript --vanilla --default-packages=methods,stats,utils -e \'setwd("{0}")\' -e \'source("{1}")\''.format(base, fname)

rule update_list:
    shell:
        source_r('R', 'read_json.R') + ' 3'
