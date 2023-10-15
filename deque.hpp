#include <iostream>
#include <vector>

template <typename T, typename Allocator = std::allocator<T>>
class Deque {
  public:
  using traits = std::allocator_traits<Allocator>;
  Deque(const Allocator& allocator = Allocator())
      : left_((block_number_ - 1) / 2, 0, this),
        right_((block_number_ - 1) / 2, 0, this),
        buffer_(block_number_),
        allocator_(allocator) {
    for (size_t ind = 0; ind < block_number_; ++ind) {
      buffer_[ind] = traits::allocate(allocator_, kBlockSize);
    }
  }

  Deque(size_t count, const Allocator& allocator = Allocator())
      : block_number_((count / kBlockSize + 1) * 2),
        left_((block_number_ - 1) / 2, 0, this),
        right_((block_number_ - 1) / 2, 0, this),
        buffer_(block_number_),
        allocator_(allocator) {
    try {
      for (size_t ind = 0; ind < block_number_; ++ind) {
        buffer_[ind] = traits::allocate(allocator_, kBlockSize);
      }
      for (size_t ind = 0; ind < count; ++ind, ++right_) {
        traits::construct(allocator_, buffer_[right_.block] + right_.pos);
      }
    } catch (...) {
      clear();
      throw;
    }
  }

  Deque(size_t count, const T& value, const Allocator& allocator = Allocator())
      : block_number_((count / kBlockSize + 1) * 2),
        left_((block_number_ - 1) / 2, 0, this),
        right_((block_number_ - 1) / 2, 0, this),
        buffer_(block_number_),
        allocator_(allocator) {
    try {
      for (size_t ind = 0; ind < block_number_; ++ind) {
        buffer_[ind] = traits::allocate(allocator_, kBlockSize);
      }
      for (size_t ind = 0; ind < count; ++ind, ++right_) {
        traits::construct(allocator_, buffer_[right_.block] + right_.pos,
                          value);
      }
    } catch (...) {
      clear();
      throw;
    }
  }

  Deque(std::initializer_list<T> init, const Allocator& allocator = Allocator())
      : block_number_(init.size()),
        left_((block_number_ - 1) / 2, 0, this),
        right_((block_number_ - 1) / 2, 0, this),
        buffer_(block_number_),
        allocator_(allocator) {
    for (size_t ind = 0; ind < block_number_; ++ind) {
      buffer_[ind] = traits::allocate(allocator_, kBlockSize);
    }
    for (auto iter = init.begin(); iter != init.end(); ++iter, ++right_) {
      traits::construct(allocator_, buffer_[right_.block] + right_.pos, *iter);
    }
  }

  Deque(const Deque& other)
      : block_number_(other.block_number_),
        left_((block_number_ - 1) / 2, 0, this),
        right_((block_number_ - 1) / 2, 0, this),
        buffer_(block_number_),
        allocator_(
            traits::select_on_container_copy_construction(other.allocator_)) {
    try {
      size_t ind = 0;
      for (; ind < block_number_; ++ind) {
        buffer_[ind] = traits::allocate(allocator_, kBlockSize);
      }
      for (ind = 0; ind < other.size(); ++ind, ++right_) {
        traits::construct(allocator_, buffer_[right_.block] + right_.pos,
                          other[ind]);
      }
    } catch (...) {
      clear();
      throw;
    }
  }

  Deque(Deque&& other)
      : buffer_(std::move(other.buffer_)),
        left_(other.left_),
        right_(other.right_),
        block_number_(other.block_number_),
        allocator_(other.allocator_) {
    left_.container = right_.container = this;
    other.right_ = other.left_ = {(block_number_ - 1) / 2, 0, &other};
  }

  Deque& operator=(Deque&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    Deque tmp(std::move(other));
    if (traits::propagate_on_container_copy_assignment::value and
        tmp.allocator_ != other.allocator_) {
      tmp.allocator_ = other.allocator_;
    }
    std::swap(allocator_, tmp.allocator_);
    std::swap(block_number_, tmp.block_number_);
    std::swap(left_, tmp.left_);
    std::swap(right_, tmp.right_);
    left_.container = right_.container = this;
    std::swap(buffer_, tmp.buffer_);
    return *this;
  }

  Deque& operator=(const Deque& other) {
    if (this == &other) {
      return *this;
    }
    Deque tmp(other);
    if (traits::propagate_on_container_copy_assignment::value and
        tmp.allocator_ != other.allocator_) {
      tmp.allocator_ = other.allocator_;
    }
    std::swap(allocator_, tmp.allocator_);
    std::swap(block_number_, tmp.block_number_);
    std::swap(left_, tmp.left_);
    std::swap(right_, tmp.right_);
    left_.container = right_.container = this;
    std::swap(buffer_, tmp.buffer_);
    return *this;
  }

  size_t size() const {
    if (right_.block == left_.block) {
      return right_.pos - left_.pos;
    }
    return (right_.block - left_.block - 1) * kBlockSize + kBlockSize -
        left_.pos + right_.pos;
  }

  T& operator[](size_t index) {
    BaseIterator<false> get_position = left_;
    get_position += index;
    return *(buffer_[get_position.block] + get_position.pos);
  }

  const T& operator[](size_t index) const {
    BaseIterator<false> get_position = left_;
    get_position += index;
    return *(buffer_[get_position.block] + get_position.pos);
  }

  T& at(size_t index) {
    if (index >= size()) {
      throw std::out_of_range("out of range");
    }
    return Deque<T>::operator[](index);
  }

  const T& at(size_t index) const {
    if (index >= size()) {
      throw std::out_of_range("out of range");
    }
    return Deque<T>::operator[](index);
  }

  Allocator get_allocator() { return allocator_; }

  void push_back(const T& value) {
    if (right_.pos == 0 && right_.block == block_number_) {
      size_t add_size = right_.block - left_.block + 1;
      for (size_t i = 0; i < add_size; ++i) {
        buffer_.insert(buffer_.end(), 1,
                       traits::allocate(allocator_, kBlockSize));
      }
      block_number_ += add_size;
    }
    size_t ind = 0;
    traits::construct(allocator_, buffer_[right_.block] + right_.pos, value);
    ++right_;
  }

  void push_back(T&& value) {
    if (right_.pos == 0 && right_.block == block_number_) {
      size_t add_size = right_.block - left_.block + 1;
      for (size_t i = 0; i < add_size; ++i) {
        buffer_.insert(buffer_.end(), 1,
                       traits::allocate(allocator_, kBlockSize));
      }
      block_number_ += add_size;
    }
    size_t ind = 0;
    traits::construct(allocator_, buffer_[right_.block] + right_.pos,
                      std::forward<T>(value));
    ++right_;
  }

  template <class... Args>
  void emplace_back(Args&&... args) {
    if (right_.pos == 0 && right_.block == block_number_) {
      size_t add_size = right_.block - left_.block + 1;
      for (size_t i = 0; i < add_size; ++i) {
        buffer_.insert(buffer_.end(), 1,
                       traits::allocate(allocator_, kBlockSize));
      }
      block_number_ += add_size;
    }
    size_t ind = 0;
    traits::construct(allocator_, buffer_[right_.block] + right_.pos,
                      std::forward<Args>(args)...);
    ++right_;
  }

  void push_front(const T& value) {
    if (left_.pos == 0 && left_.block == 0) {
      size_t add_size = right_.block - left_.block + 1;
      for (size_t i = 0; i < add_size; ++i) {
        buffer_.insert(buffer_.end(), 1,
                       traits::allocate(allocator_, kBlockSize));
      }
      block_number_ += add_size;
      left_.block += add_size;
      right_.block += add_size;
    }

    --left_;
    traits::construct(allocator_, buffer_[left_.block] + left_.pos, value);
  }

  void push_front(T&& value) {
    if (left_.pos == 0 && left_.block == 0) {
      size_t add_size = right_.block - left_.block + 1;
      for (size_t i = 0; i < add_size; ++i) {
        buffer_.insert(buffer_.end(), 1,
                       traits::allocate(allocator_, kBlockSize));
      }
      block_number_ += add_size;
      left_.block += add_size;
      right_.block += add_size;
    }

    --left_;
    traits::construct(allocator_, buffer_[left_.block] + left_.pos,
                      std::forward<T>(value));
  }

  template <class... Args>
  void emplace_front(Args&&... args) {
    if (right_.pos == 0 && right_.block == block_number_) {
      size_t add_size = right_.block - left_.block + 1;
      for (size_t i = 0; i < add_size; ++i) {
        buffer_.insert(buffer_.end(), 1,
                       traits::allocate(allocator_, kBlockSize));
      }
      block_number_ += add_size;
    }
    size_t ind = 0;
    --left_;
    traits::construct(allocator_, buffer_[left_.block] + left_.pos,
                      std::forward<Args>(args)...);
  }

  void pop_back() {
    traits::destroy(allocator_, buffer_[right_.block] + right_.pos);
    --right_;
  }

  void pop_front() {
    traits::destroy(allocator_, buffer_[left_.block] + left_.pos);
    ++left_;
  }

  bool empty() const { return size() == 0; };

  template <bool IsConst>
  struct BaseIterator {
    operator BaseIterator<true>() const {
      return BaseIterator<true>(block, pos);
    }

    using difference_type = std::ptrdiff_t;
    using value_type = typename std::conditional<IsConst, const T, T>::type;
    using pointer = typename std::conditional<IsConst, const T*, T*>::type;
    using reference = typename std::conditional<IsConst, const T&, T&>::type;
    using iterator_category = std::random_access_iterator_tag;

    BaseIterator(size_t block, size_t pos) : block(block), pos(pos) {}
    BaseIterator(size_t block, size_t pos, Deque<T, Allocator>* container)
        : block(block), pos(pos), container(container) {}
    size_t block;
    size_t pos;
    Deque<T, Allocator>* container;

    BaseIterator& operator++() {
      if (pos == kBlockSize - 1) {
        ++block;
        pos = 0;
        return *this;
      }
      ++pos;
      return *this;
    }

    BaseIterator& operator--() {
      if (pos == 0) {
        --block;
        pos = kBlockSize - 1;
        return *this;
      }
      --pos;
      return *this;
    }

    BaseIterator operator++(int) {
      BaseIterator res = *this;
      this->operator++();
      return res;
    }

    BaseIterator operator--(int) {
      BaseIterator res = *this;
      this->operator--();
      return res;
    }

    BaseIterator& operator+=(size_t delta) {
      block = (block * kBlockSize + pos + delta) / kBlockSize;
      pos = (pos + delta) % kBlockSize;
      return *this;
    }
    BaseIterator& operator-=(size_t delta) {
      *this += (-delta);
      return *this;
    }
    BaseIterator operator+(size_t delta) const {
      BaseIterator update = *this;
      update += delta;
      return update;
    }
    BaseIterator operator-(size_t delta) const {
      BaseIterator update = *this;
      update -= delta;
      return update;
    }

    bool operator<(const BaseIterator& other) const {
      return block < other.block || (block == other.block && pos < other.pos);
    }
    bool operator>(const BaseIterator& other) const { return other < *this; }
    bool operator<=(const BaseIterator& other) const {
      return !(*this > other);
    }
    bool operator>=(const BaseIterator& other) const {
      return !(*this < other);
    }
    bool operator==(const BaseIterator& other) const {
      return *this <= other && *this >= other;
    }
    bool operator!=(const BaseIterator& other) const {
      return !(*this == other);
    }

    difference_type operator-(const BaseIterator& other) const {
      if (block == other.block) {
        return pos - other.pos;
      }
      return (block - other.block - 1) * kBlockSize + kBlockSize - other.pos +
          pos;
    }

    reference operator*() { return *((container->buffer_)[block] + pos); }
    pointer operator->() const { return ((container->buffer_)[block] + pos); }
  };

  using iterator = BaseIterator<false>;
  using const_iterator = BaseIterator<true>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  iterator begin() { return left_; }

  const_iterator begin() const { return left_; }

  iterator end() { return right_; }

  const_iterator end() const { return right_; }

  const_iterator cbegin() const { return left_; }

  const_iterator cend() const { return right_; }

  reverse_iterator rbegin() { return std::make_reverse_iterator(end()); }

  const_reverse_iterator rbegin() const {
    return std::make_reverse_iterator(end());
  }

  reverse_iterator rend() { return std::make_reverse_iterator(begin()); }

  const_reverse_iterator rend() const {
    return std::make_reverse_iterator(begin());
  }

  const_reverse_iterator crbegin() const {
    return std::make_reverse_iterator(cend());
  }

  const_reverse_iterator crend() const {
    return std::make_reverse_iterator(cbegin());
  }

  void insert(iterator target, const T& value) {
    if (target == begin()) {
      push_front(value);
      return;
    }
    push_back(*(end() - 1));
    for (iterator shift = end() - 2; shift > target; --shift) {
      *shift = *(shift - 1);
    }
    *target = value;
  }

  void emplace(iterator target, T&& value) {
    if (target == begin()) {
      emplace_front(value);
      return;
    }
    emplace_back(*(end() - 1));
    for (iterator shift = end() - 2; shift > target; --shift) {
      *shift = *(shift - 1);
    }
    *target = value;
  }

  void erase(iterator target) {
    if (target == begin()) {
      pop_front();
      return;
    }
    for (iterator shift = target + 1; shift < end(); ++shift) {
      *(shift - 1) = *shift;
    }
    pop_back();
  }

  void clear() {
    for (T& elem : *this) {
      traits::destroy(allocator_, &elem);
    }
    for (auto& block : buffer_) {
      traits::deallocate(allocator_, block, kBlockSize);
    }
  }

  ~Deque() { clear(); }

  private:
  Allocator allocator_;
  const size_t kBlockNumber = 10;
  size_t block_number_ = kBlockNumber;
  static const size_t kBlockSize = 32;
  BaseIterator<false> left_;
  BaseIterator<false> right_;
  std::vector<T*> buffer_;
};