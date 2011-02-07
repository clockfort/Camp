
#include <curl/curl.h>

CURLcode
curl_easy_setopt_long(CURL *handle, CURLoption option, long parameter) {
    curl_easy_setopt(handle, option, parameter);
}

CURLcode
curl_easy_setopt_ptr(CURL *handle, CURLoption option, void *parameter) {
    curl_easy_setopt(handle, option, parameter);
}

CURLcode
curl_easy_setopt_funptr(CURL *handle, CURLoption option, void *parameter) {
    curl_easy_setopt(handle, option, parameter);
}

CURLcode
curl_easy_setopt_off(CURL *handle, CURLoption option, curl_off_t parameter) {
    curl_easy_setopt(handle, option, parameter);
}
