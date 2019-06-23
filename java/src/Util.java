package de.wwu.is.RAutoWEKA;

import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

public abstract class Util {

    /**
     * Populates an Instances object with given data in column-major format.
     * <p>
     * Adapted from RWeka (0.4-40)
     * Authors: Kurt Hornik [aut, cre], Christian Buchta [ctb], Torsten Hothorn [ctb], Alexandros Karatzoglou [ctb], David Meyer [ctb], Achim Zeileis [ctb]
     * Link: https://cran.r-project.org/package=RWeka
     *
     * @param instances    The Instances object to populate.
     * @param data         The data in column-major format.
     * @param numberOfRows The number of rows.
     * @throws IllegalArgumentException If the number of rows does not match the data.
     */
    public static void addInstances(Instances instances, double[] data, int numberOfRows) throws Exception {
        int i, j, numberOfColumns = instances.numAttributes();

        if (data.length / numberOfColumns != numberOfRows) {
            throw new IllegalArgumentException("Number of rows does not match the given data");
        }

        for (i = 0; i < numberOfRows; i++) {
            Instance instance = new DenseInstance(numberOfColumns);
            for (j = 0; j < numberOfColumns; j++)
                instance.setValue(j, data[i + j * numberOfRows]);
            instances.add(instance);
        }
    }

    /**
     * Parses dates given as string array to a double array.
     *
     * @param attribute   A reference attribute.
     * @param data        The data to parse.
     * @param naCharacter The character indicating NAs.
     * @return A double array representation of the dates.
     */
    public static double[] parseDate(Attribute attribute, String[] data, String naCharacter) {
        double[] result = new double[data.length];

        for (int i = 0; i < data.length; i++) {
            try {
                if (data[i].equals(naCharacter)) {
                    result[i] = Double.NaN;
                } else {
                    result[i] = attribute.parseDate(data[i]);
                }
            } catch (Exception exception) {
                result[i] = Double.NaN;
            }
        }

        return result;
    }

}
