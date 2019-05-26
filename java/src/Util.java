package de.wwu.is.RAutoWEKA;

import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

// Adapted from RWeka (0.4-40)
// Authors: Kurt Hornik [aut, cre], Christian Buchta [ctb], Torsten Hothorn [ctb], Alexandros Karatzoglou [ctb], David Meyer [ctb], Achim Zeileis [ctb]
// Link: https://cran.r-project.org/package=RWeka
public abstract class Util {

    // Populate Instances with data in column-major format. Use
    // with care if called more than once with the same Instances
    // object. (C)
    //
    // Instance is now an Interface, Weka >= 3-7-1. The equivalent
    // class seems to be DenseInstance.
    public static void addInstances(Instances instances, double[] data, int nrow) throws Exception {
        int i, j, ncol = instances.numAttributes();

        if (data.length / ncol != nrow) {
            throw new Exception("invalid number of rows 'nrow'");
        }

        for (i = 0; i < nrow; i++) {
            Instance instance = new DenseInstance(ncol);
            for (j = 0; j < ncol; j++)
                instance.setValue(j, data[i + j * nrow]);
            instances.add(instance);
        }
    }

    public static double[] parseDate(Attribute A, String[] data, String NA_character) {
        double[] out = new double[data.length];
        for (int i = 0; i < data.length; i++) {
            try {
                if (data[i].equals(NA_character)) {
                    out[i] = Double.NaN;
                } else {
                    out[i] = A.parseDate(data[i]);
                }
            } catch (Exception e) {
                out[i] = Double.NaN;
            }
        }
        return out;
    }

}
