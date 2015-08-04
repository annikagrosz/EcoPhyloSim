/*
 * StringConversion.h
 *
 *  Created on: 30.07.2015
 *      Author: Betim Musa <musab@informatik.uni-freiburg.de>
 */

#include <sstream>
#include <stdexcept>
#include <string>

#ifndef STRINGCONVERSION_H_
#define STRINGCONVERSION_H_

class conversion_error : public std::runtime_error {
public:
   explicit conversion_error (const std::string& what_arg) : std::runtime_error(what_arg) {}
   explicit conversion_error (const char* what_arg) : std::runtime_error(what_arg) {}
};

template <typename T>
inline std::string to_string(T number) {
   std::stringstream ss;
   ss << number;
   return ss.str();
}

inline int str_to_int(const std::string& numAsString) {
   int result;
   std::istringstream ss(numAsString);
   // In case of an error during conversion, throw an error.
   if (!(ss >> result)) throw new conversion_error(std::string("Conversion to integer of \" ")
      + numAsString + std::string("\" failed."));
   return result;
}
inline int str_to_int(const char* numAsString) {
   int result;
   std::istringstream ss(numAsString);
   // In case of an error during conversion, throw an error.
   if (!(ss >> result)) throw new conversion_error(std::string("Conversion to integer of \" ")
      + std::string(numAsString) + std::string("\" failed."));
   return result;
}

inline double str_to_double(const std::string& numAsString) {
   double result;
   std::istringstream ss(numAsString);
   // In case of an error during conversion, throw an error.
   if (!(ss >> result)) throw new conversion_error(std::string("Conversion to double of \" ")
      + numAsString + std::string("\" failed."));
   return result;
}

inline double str_to_double(const char* numAsString) {
   double result;
   std::istringstream ss(numAsString);
   // In case of an error during conversion, throw an error.
   if (!(ss >> result)) throw new conversion_error(std::string("Conversion to double of \" ")
      + std::string(numAsString) + std::string("\" failed."));
   return result;
}

#endif /* STRINGCONVERSION_H_ */
