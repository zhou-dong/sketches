# Theta Sketch

Theta Sketch are a type of probabilistic data structure designed for approximate set operations, such as estimating the cardinality (number of unique elements) and performing set operations (union, intersection, difference). They are based on the idea of maintaining a subset of the input data, using hashing to determine which elements to include in this subset.

Key characteristics of Theta Sketches include:

- Accuracy vs. Space Trade-off: The accuracy of the estimation can be controlled by the size of the sketch, allowing a balance between memory usage and precision.
- Mergeable: Theta Sketches can be merged, making them suitable for distributed systems where data is processed in parallel across multiple nodes.
